suppressMessages(nsqipr::nsqip("test-data", csv = NA))

withr::defer({
  fs::file_move(fs::dir_ls(file.path("test-data","acs_nsqip_puf"), type = "file"),"test-data")
  fs::dir_delete(file.path("test-data","acs_nsqip_puf"))

  fs::file_move(fs::dir_ls(file.path("test-data","puf_tar_col"), type = "file"),"test-data")
  fs::dir_delete(file.path("test-data","puf_tar_col"))

  fs::file_move(fs::dir_ls(file.path("test-data","puf_tar_app"), type = "file"),"test-data")
  fs::dir_delete(file.path("test-data","puf_tar_app"))
}, teardown_env())
