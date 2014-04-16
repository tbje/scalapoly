package tbje.web.templates

import tbje.facelift.attr.Id

object Ids {
  val dropDownField = Id("dropDownField")
  val gConnectId = Id("gConnect")
  val gConnectWait = Id("gConnect-wait")
  val transferImage = Id("transfer")
  val noticeId = Id("notice")
  val detailsForm = Id("detailsForm")
  val loggedInContent = Id("loggedInContent")
  val submitId = Id("submitId")
  val mainMenuCollapse = Id("main-menu-collapse")
  val progressBar = Id("progress-bar")
  val progressDiv = Id("progress-div")
  val fileUpload = Id("fileupload")
  val fileUploadDiv = Id("fileupload-div")
  val uploadHistoryId = Id("upload-history")
  val uploadHistoryTableBody = Id("upload-history-table-body")
  val uploadHistoryNoRows = Id("upload-history-no-rows")

  val List(
    navbarDivId,
    contentDivId,
    zermatImageId,
    headingDiv,
    bannerDiv,
    openIdDiv,
    openIdButton,
    openIdField,
    loginDiv,
    logInButtonId) = List(
    "navbarDiv",
    "contetentDiv",
    "zermatImage",
    "headingDivId",
    "bannerDiv",
    "openIdDiv",
    "openIdButton",
    "openIdField",
    "loginDiv",
    "logInButton").map(Id)

}
