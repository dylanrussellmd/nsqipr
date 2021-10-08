nsqipr::nsqip("test-data", csv = NA)

withr::defer({
  fs::file_move(fs::dir_ls(file.path("test-data","acs_nsqip_puf"), type = "file"),"test-data")
  fs::dir_delete(file.path("test-data","acs_nsqip_puf"))
}, teardown_env())
