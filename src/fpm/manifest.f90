!> Package configuration data.
!>
!> This module provides the necessary procedure to translate a TOML document
!> to the corresponding Fortran type, while verifying it with respect to
!> its schema.
!>
!> Additionally, the required data types for users of this module are reexported
!> to hide the actual implementation details.
module fpm_manifest
    use,intrinsic :: iso_fortran_env, only : int64
    use fpm_manifest_example, only : example_config_t
    use fpm_manifest_executable, only : executable_config_t
    use fpm_manifest_dependency, only : dependency_config_t
    use fpm_manifest_library, only : library_config_t
    use fpm_manifest_preprocess, only : preprocess_config_t
    use fpm_manifest_package, only : package_config_t, new_package
    use fpm_error, only : error_t, fatal_error
    use tomlf, only : toml_table
    use fpm_toml, only : read_package_file
    use fpm_manifest_test, only : test_config_t
    use fpm_filesystem, only: join_path, exists, dirname, is_dir, canon_path, file_info
    use fpm_environment, only: os_is_unix
    use fpm_strings, only: string_t
    implicit none
    private

    public :: get_package_data, default_executable, default_library, default_test
    public :: get_package_dependencies
    public :: default_example
    public :: package_config_t, dependency_config_t, preprocess_config_t

    !> A single cached manifest
    type, private :: manifest_cache_entry_t
        !> Canonical manifest path (cache key)
        character(:), allocatable :: key
        !> File size in bytes when the entry was recorded
        integer(int64) :: size = -1
        !> File modification time when the entry was recorded
        integer(int64) :: mtime = -1
        !> Manifest without defaults applied
        type(package_config_t) :: config
    end type manifest_cache_entry_t

    !> Cache of parsed package manifests
    !>
    !> The same `fpm.toml` is read several times during a single invocation
    !> (every dependency graph iteration, then again when the model is built).
    !> Entries are keyed by canonical path and validated with size and
    !> modification time, so a changed manifest is parsed again.
    type, public :: manifest_cache_t
        private
        !> Cached manifests
        type(manifest_cache_entry_t), allocatable :: entries(:)
        !> Number of valid entries in `entries`
        integer :: n_entries = 0
    contains
        !> Return the manifest of a file, parsing it at most once
        procedure :: load => manifest_cache_load
    end type manifest_cache_t


contains


    !> Populate library in case we find the default src directory
    subroutine default_library(self)

        !> Instance of the library meta data
        type(library_config_t), intent(out) :: self

        self%source_dir = "src"
        self%include_dir = [string_t("include")]
        self%lib_type = [string_t("monolithic")]

    end subroutine default_library


    !> Populate executable in case we find the default app directory
    subroutine default_executable(self, name)

        !> Instance of the executable meta data
        type(executable_config_t), intent(out) :: self

        !> Name of the package
        character(len=*), intent(in) :: name

        self%name = name
        self%source_dir = "app"
        self%main = "main.f90"

    end subroutine default_executable

    !> Populate test in case we find the default example/ directory
    subroutine default_example(self, name)

        !> Instance of the executable meta data
        type(example_config_t), intent(out) :: self

        !> Name of the package
        character(len=*), intent(in) :: name

        self%name = name // "-demo"
        self%source_dir = "example"
        self%main = "main.f90"

    end subroutine default_example

    !> Populate test in case we find the default test/ directory
    subroutine default_test(self, name)

        !> Instance of the executable meta data
        type(test_config_t), intent(out) :: self

        !> Name of the package
        character(len=*), intent(in) :: name

        self%name = name // "-test"
        self%source_dir = "test"
        self%main = "main.f90"

    end subroutine default_test


    !> Obtain package meta data from a configuation file
    subroutine get_package_data(package, file, error, apply_defaults)

        !> Parsed package meta data
        type(package_config_t), intent(out) :: package

        !> Name of the package configuration file
        character(len=*), intent(in) :: file

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        !> Apply package defaults (uses file system operations)
        logical, intent(in), optional :: apply_defaults

        logical :: defaults

        defaults = .false.
        if (present(apply_defaults)) defaults = apply_defaults

        call read_manifest(package, file, error)
        if (allocated(error)) return

        call apply_manifest_defaults(package, file, defaults, error)

    end subroutine get_package_data

    !> Obtain package meta data, reusing an earlier parse of the same file
    subroutine manifest_cache_load(self, package, file, error, apply_defaults)

        !> Instance of the manifest cache
        class(manifest_cache_t), intent(inout) :: self

        !> Parsed package meta data
        type(package_config_t), intent(out) :: package

        !> Name of the package configuration file
        character(len=*), intent(in) :: file

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        !> Apply package defaults (uses file system operations)
        logical, intent(in), optional :: apply_defaults

        integer(int64) :: size, mtime
        integer :: i, pos
        character(:), allocatable :: key
        logical :: inspectable, defaults

        defaults = .false.
        if (present(apply_defaults)) defaults = apply_defaults

        inspectable = file_info(file, size, mtime)
        if (inspectable) key = canon_path(file)

        pos = 0
        if (inspectable) then
            do i = 1, self%n_entries
                if (self%entries(i)%key /= key) cycle
                pos = i
                exit
            end do
            if (pos > 0) then
                if (self%entries(pos)%size == size .and. self%entries(pos)%mtime == mtime) then
                    package = self%entries(pos)%config
                    call apply_manifest_defaults(package, file, defaults, error)
                    return
                end if
            end if
        end if

        call read_manifest(package, file, error)
        if (allocated(error)) return

        if (inspectable) then
            if (pos == 0) then
                call ensure_manifest_capacity(self, self%n_entries + 1)
                self%n_entries = self%n_entries + 1
                pos = self%n_entries
                self%entries(pos)%key = key
            end if
            self%entries(pos)%size = size
            self%entries(pos)%mtime = mtime
            self%entries(pos)%config = package
        end if

        call apply_manifest_defaults(package, file, defaults, error)

    end subroutine manifest_cache_load

    !> Read and parse a manifest without applying defaults
    subroutine read_manifest(package, file, error)

        !> Parsed package meta data
        type(package_config_t), intent(out) :: package

        !> Name of the package configuration file
        character(len=*), intent(in) :: file

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), allocatable :: table

        call read_package_file(table, file, error)
        if (allocated(error)) return

        if (.not. allocated(table)) then
            call fatal_error(error, "Unclassified error while reading: '"//file//"'")
            return
        end if

        call new_package(package, table, dirname(file), error)

    end subroutine read_manifest

    !> Apply package defaults, when requested
    subroutine apply_manifest_defaults(package, file, enabled, error)

        !> Package meta data to complete
        type(package_config_t), intent(inout) :: package

        !> Name of the package configuration file
        character(len=*), intent(in) :: file

        !> Whether defaults should be applied
        logical, intent(in) :: enabled

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: root

        if (.not. enabled) return

        root = dirname(file)
        if (len_trim(root) == 0) root = "."
        call package_defaults(package, root, error)

    end subroutine apply_manifest_defaults

    !> Grow the cache storage to hold at least `min_capacity` entries
    subroutine ensure_manifest_capacity(cache, min_capacity)

        !> Instance of the manifest cache
        type(manifest_cache_t), intent(inout) :: cache

        !> Required number of entries
        integer, intent(in) :: min_capacity

        type(manifest_cache_entry_t), allocatable :: tmp(:)
        integer :: new_capacity

        if (min_capacity <= 0) return
        if (allocated(cache%entries)) then
            if (size(cache%entries) >= min_capacity) return
        end if

        new_capacity = 8
        if (allocated(cache%entries)) new_capacity = 2 * size(cache%entries)
        do while (new_capacity < min_capacity)
            new_capacity = 2 * new_capacity
        end do

        allocate (tmp(new_capacity))
        if (cache%n_entries > 0) tmp(1:cache%n_entries) = cache%entries(1:cache%n_entries)
        call move_alloc(tmp, cache%entries)

    end subroutine ensure_manifest_capacity


    !> Apply package defaults
    subroutine package_defaults(package, root, error)

        !> Parsed package meta data
        type(package_config_t), intent(inout) :: package

        !> Current working directory
        character(len=*), intent(in) :: root

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        ! Populate library in case we find the default src directory
        if (.not.allocated(package%library) .and. &
            & (is_dir(join_path(root, "src")) .or. &
            &  is_dir(join_path(root, "include")))) then

            allocate(package%library)
            call default_library(package%library)
        end if

        ! Populate executable in case we find the default app
        if (.not.allocated(package%executable) .and. &
            & exists(join_path(root, "app", "main.f90"))) then
            allocate(package%executable(1))
            call default_executable(package%executable(1), package%name)
        end if

        ! Populate example in case we find the default example directory
        if (.not.allocated(package%example) .and. &
            & exists(join_path(root, "example", "main.f90"))) then
            allocate(package%example(1))
            call default_example(package%example(1), package%name)
        endif

        ! Populate test in case we find the default test directory
        if (.not.allocated(package%test) .and. &
            & exists(join_path(root, "test", "main.f90"))) then
            allocate(package%test(1))
            call default_test(package%test(1), package%name)
        endif

        if (.not.(allocated(package%library) &
            & .or. allocated(package%executable) &
            & .or. allocated(package%example) &
            & .or. allocated(package%test))) then
            call fatal_error(error, "Neither library nor executable found, there is nothing to do")
            return
        end if

    end subroutine package_defaults
    
    ! Return an array of all dependencies in the manifest
    subroutine get_package_dependencies(package, main, deps)

        !> Parsed package meta data
        type(package_config_t), intent(in) :: package
        
        !> Is the main project
        logical, intent(in) :: main        
        
        !> Unprocessed list of all dependencies listed in this manifest
        class(dependency_config_t), allocatable, intent(inout) :: deps(:)
        
        integer :: ndeps,k
        
        if (allocated(deps)) deallocate(deps)

        ndeps = 0
        if (allocated(package%dependency)) &
        ndeps = ndeps + size(package%dependency)
        
        if (main) then 
        
            if (allocated(package%dev_dependency)) &
            ndeps = ndeps + size(package%dev_dependency)
                    
            if (allocated(package%example)) then
               do k = 1, size(package%example)
                  if (allocated(package%example(k)%dependency)) &
                  ndeps = ndeps + size(package%example(k)%dependency)
               end do
            end if

            if (allocated(package%executable)) then
               do k = 1, size(package%executable)
                  if (allocated(package%executable(k)%dependency)) &
                  ndeps = ndeps + size(package%executable(k)%dependency)
               end do
            end if
            
            if (allocated(package%test)) then
               do k = 1, size(package%test)
                  if (allocated(package%test(k)%dependency)) &
                  ndeps = ndeps + size(package%test(k)%dependency)
               end do
            end if     
        
        endif   
        
        allocate(deps(ndeps))
        
        if (ndeps > 0) then
           
           ndeps = 0
           
           if (allocated(package%dependency)) &
           call collect(deps,ndeps,package%dependency)
           
           if (main) then 
           
               if (allocated(package%dev_dependency)) &
               call collect(deps,ndeps,package%dev_dependency)
               
               if (allocated(package%example)) then
                  do k = 1, size(package%example)
                     if (allocated(package%example(k)%dependency)) &
                     call collect(deps,ndeps,package%example(k)%dependency)
                  end do
               end if
               if (allocated(package%executable)) then
                  do k = 1, size(package%executable)
                     if (allocated(package%executable(k)%dependency)) &
                     call collect(deps,ndeps,package%executable(k)%dependency)
                  end do
               end if
               if (allocated(package%test)) then
                  do k = 1, size(package%test)
                     if (allocated(package%test(k)%dependency)) &
                     call collect(deps,ndeps,package%test(k)%dependency)
                  end do
               end if    
           
           endif
         
         endif

      contains
      
         ! Add dependencies to the list      
         subroutine collect(list, nreq, new_deps)
            type(dependency_config_t), intent(inout) :: list(:)
            integer,                   intent(inout) :: nreq
            type(dependency_config_t), intent(in)    :: new_deps(:)
            
            integer :: i
            do i = 1, size(new_deps)
               nreq = nreq + 1
               list(nreq) = new_deps(i)
            end do
         end subroutine collect
        
    end subroutine get_package_dependencies


end module fpm_manifest
