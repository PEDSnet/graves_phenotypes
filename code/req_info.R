#!/usr/bin/env Rscript

#' Administrative information about request.
#'
#' The configuration items present in the template should be completed
#' for all requests.
#'
#' @md
"req_info"

config('title',
       'Graves')
config('requester_name', 'Julia Donner')
config('requester_site', 'Stanford')
config('cdm', 'v54')
config('cdm_version', 1)
config('enqueued_date', as.Date('2024-09-02'))

config('req_version', 1)
config('req_basename', '--help')

# In most cases, these assignments should not need to be changed.
# But if for some reason you have altered the layout of the request
# package, you will need to edit these accordingly:
#
#  * code_dir = location of code for request
#  * spec_dir = location of data files needed to execute the request
#  * local_dir = location to which intermediate results may be saved
#    for local inspection
#  * result_dir = location to which results to be returned to the DCC
#    will be written
#
# N.B.  If you change code_dir, you must either source this file
# prior to util.R (the usual case), or edit util.R and driver.R to
# reflect the change.
config('subdirs',
       list(code_dir = 'code',
            site_dir = 'site',
            spec_dir = 'specs',
            local_dir = 'local',
            result_dir = 'results'))

# This should not be changed if the request was set up by the
# new_request script.  If you are setting up manually, edit this
# to reflect the version of the standard framework used.
config('framework_version', '03c3d1b 2024-05-14')
