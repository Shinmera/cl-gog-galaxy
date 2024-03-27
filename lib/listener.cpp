#include <galaxy/GalaxyApi.h>
#include <cstring>
#include "gog.h"

using namespace galaxy::api;

typedef gog_Interface (*makefun)(struct gog_listener *listener);
typedef void (*freefun)(gog_Interface listener);

makefun gog_listener_make[GOG_LISTENER_TYPE_END] = {0};
freefun gog_listener_free[GOG_LISTENER_TYPE_END] = {0};

GOG_EXPORT gog_Interface gog_MakeListener(gog_listener *listener, enum gog_ListenerType type){
  return gog_listener_make[type](listener);
}

GOG_EXPORT void gog_FreeListener(gog_Interface listener, enum gog_ListenerType type){
  gog_listener_free[type](listener);
}

class _IIsDlcOwnedListener : public galaxy::api::IIsDlcOwnedListener {
public:
  struct gog_listener listener;

  void OnDlcCheckFailure(ProductID productId, galaxy::api::IIsDlcOwnedListener::FailureReason failureReason) override {
    if(!this->listener.OnDlcCheckFailure) return;
    this->listener.OnDlcCheckFailure(this->listener.userptr, (gog_ProductID)productId, (enum gog_DlcCheckFailureReason)failureReason);
  }

  void OnDlcCheckSuccess(ProductID productId, bool isOwned) override {
    if(!this->listener.OnDlcCheckSuccess) return;
    this->listener.OnDlcCheckSuccess(this->listener.userptr, (gog_ProductID)productId, (bool)isOwned);
  }
};

gog_Interface make_IIsDlcOwnedListener(struct gog_listener *listener){
  _IIsDlcOwnedListener *list = new _IIsDlcOwnedListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IIsDlcOwnedListener(gog_Interface listener){
  delete (_IIsDlcOwnedListener*)listener;
}

class _IChatRoomWithUserRetrieveListener : public galaxy::api::IChatRoomWithUserRetrieveListener {
public:
  struct gog_listener listener;

  void OnChatRoomWithUserRetrieveFailure(GalaxyID userID, galaxy::api::IChatRoomWithUserRetrieveListener::FailureReason failureReason) override {
    if(!this->listener.OnChatRoomWithUserRetrieveFailure) return;
    this->listener.OnChatRoomWithUserRetrieveFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_ChatUserRetrieveFailureReason)failureReason);
  }

  void OnChatRoomWithUserRetrieveSuccess(GalaxyID userID, ChatRoomID chatRoomID) override {
    if(!this->listener.OnChatRoomWithUserRetrieveSuccess) return;
    this->listener.OnChatRoomWithUserRetrieveSuccess(this->listener.userptr, (gog_ID)userID.ToUint64(), (gog_ChatRoomID)chatRoomID);
  }
};

gog_Interface make_IChatRoomWithUserRetrieveListener(struct gog_listener *listener){
  _IChatRoomWithUserRetrieveListener *list = new _IChatRoomWithUserRetrieveListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IChatRoomWithUserRetrieveListener(gog_Interface listener){
  delete (_IChatRoomWithUserRetrieveListener*)listener;
}

class _IChatRoomMessageSendListener : public galaxy::api::IChatRoomMessageSendListener {
public:
  struct gog_listener listener;

  void OnChatRoomMessageSendFailure(ChatRoomID chatRoomID, uint32_t sentMessageIndex, galaxy::api::IChatRoomMessageSendListener::FailureReason failureReason) override {
    if(!this->listener.OnChatRoomMessageSendFailure) return;
    this->listener.OnChatRoomMessageSendFailure(this->listener.userptr, (gog_ChatRoomID)chatRoomID, (uint32_t)sentMessageIndex, (enum gog_ChatMessageSendFailureReason)failureReason);
  }

  void OnChatRoomMessageSendSuccess(ChatRoomID chatRoomID, uint32_t sentMessageIndex, ChatMessageID messageID, uint32_t sendTime) override {
    if(!this->listener.OnChatRoomMessageSendSuccess) return;
    this->listener.OnChatRoomMessageSendSuccess(this->listener.userptr, (gog_ChatRoomID)chatRoomID, (uint32_t)sentMessageIndex, (gog_ChatMessageID)messageID, (uint32_t)sendTime);
  }
};

gog_Interface make_IChatRoomMessageSendListener(struct gog_listener *listener){
  _IChatRoomMessageSendListener *list = new _IChatRoomMessageSendListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IChatRoomMessageSendListener(gog_Interface listener){
  delete (_IChatRoomMessageSendListener*)listener;
}

class _IChatRoomMessagesListener : public galaxy::api::IChatRoomMessagesListener {
public:
  struct gog_listener listener;

  void OnChatRoomMessagesReceived(ChatRoomID chatRoomID, uint32_t messageCount, uint32_t longestMessageLenght) override {
    if(!this->listener.OnChatRoomMessagesReceived) return;
    this->listener.OnChatRoomMessagesReceived(this->listener.userptr, (gog_ChatRoomID)chatRoomID, (uint32_t)messageCount, (uint32_t)longestMessageLenght);
  }
};

gog_Interface make_IChatRoomMessagesListener(struct gog_listener *listener){
  _IChatRoomMessagesListener *list = new _IChatRoomMessagesListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IChatRoomMessagesListener(gog_Interface listener){
  delete (_IChatRoomMessagesListener*)listener;
}

class _IChatRoomMessagesRetrieveListener : public galaxy::api::IChatRoomMessagesRetrieveListener {
public:
  struct gog_listener listener;

  void OnChatRoomMessagesRetrieveFailure(ChatRoomID chatRoomID, galaxy::api::IChatRoomMessagesRetrieveListener::FailureReason failureReason) override {
    if(!this->listener.OnChatRoomMessagesRetrieveFailure) return;
    this->listener.OnChatRoomMessagesRetrieveFailure(this->listener.userptr, (gog_ChatRoomID)chatRoomID, (enum gog_ChatMessageRetrieveFailureReason)failureReason);
  }

  void OnChatRoomMessagesRetrieveSuccess(ChatRoomID chatRoomID, uint32_t messageCount, uint32_t longestMessageLenght) override {
    if(!this->listener.OnChatRoomMessagesRetrieveSuccess) return;
    this->listener.OnChatRoomMessagesRetrieveSuccess(this->listener.userptr, (gog_ChatRoomID)chatRoomID, (uint32_t)messageCount, (uint32_t)longestMessageLenght);
  }
};

gog_Interface make_IChatRoomMessagesRetrieveListener(struct gog_listener *listener){
  _IChatRoomMessagesRetrieveListener *list = new _IChatRoomMessagesRetrieveListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IChatRoomMessagesRetrieveListener(gog_Interface listener){
  delete (_IChatRoomMessagesRetrieveListener*)listener;
}

class _ICloudStorageGetFileListListener : public galaxy::api::ICloudStorageGetFileListListener {
public:
  struct gog_listener listener;

  void OnGetFileListFailure(galaxy::api::ICloudStorageGetFileListListener::FailureReason failureReason) override {
    if(!this->listener.OnGetFileListFailure) return;
    this->listener.OnGetFileListFailure(this->listener.userptr, (enum gog_GetFileListFailureReason)failureReason);
  }

  void OnGetFileListSuccess(uint32_t fileCount, uint32_t quota, uint32_t quotaUsed) override {
    if(!this->listener.OnGetFileListSuccess) return;
    this->listener.OnGetFileListSuccess(this->listener.userptr, (uint32_t)fileCount, (uint32_t)quota, (uint32_t)quotaUsed);
  }
};

gog_Interface make_ICloudStorageGetFileListListener(struct gog_listener *listener){
  _ICloudStorageGetFileListListener *list = new _ICloudStorageGetFileListListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ICloudStorageGetFileListListener(gog_Interface listener){
  delete (_ICloudStorageGetFileListListener*)listener;
}

class _ICloudStorageGetFileListener : public galaxy::api::ICloudStorageGetFileListener {
public:
  struct gog_listener listener;

  void OnGetFileFailure(const char* container, const char* name, galaxy::api::ICloudStorageGetFileListener::FailureReason failureReason) override {
    if(!this->listener.OnGetFileFailure) return;
    this->listener.OnGetFileFailure(this->listener.userptr, (const char*)container, (const char*)name, (enum gog_GetFileFailureReason)failureReason);
  }

  void OnGetFileSuccess( const char* container, const char* name, uint32_t fileSize, SavegameType savegameType, const char* savegameID) override {
    if(!this->listener.OnGetFileSuccess) return;
    this->listener.OnGetFileSuccess(this->listener.userptr, (const char*)container, (const char*)name, (uint32_t)fileSize, (enum gog_SavegameType)savegameType, (const char*)savegameID);
  }
};

gog_Interface make_ICloudStorageGetFileListener(struct gog_listener *listener){
  _ICloudStorageGetFileListener *list = new _ICloudStorageGetFileListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ICloudStorageGetFileListener(gog_Interface listener){
  delete (_ICloudStorageGetFileListener*)listener;
}

class _ICloudStoragePutFileListener : public galaxy::api::ICloudStoragePutFileListener {
public:
  struct gog_listener listener;

  void OnPutFileFailure(const char* container, const char* name, galaxy::api::ICloudStoragePutFileListener::FailureReason failureReason) override {
    if(!this->listener.OnPutFileFailure) return;
    this->listener.OnPutFileFailure(this->listener.userptr, (const char*)container, (const char*)name, (enum gog_PutFileFailureReason)failureReason);
  }

  void OnPutFileSuccess(const char* container, const char* name) override {
    if(!this->listener.OnPutFileSuccess) return;
    this->listener.OnPutFileSuccess(this->listener.userptr, (const char*)container, (const char*)name);
  }
};

gog_Interface make_ICloudStoragePutFileListener(struct gog_listener *listener){
  _ICloudStoragePutFileListener *list = new _ICloudStoragePutFileListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ICloudStoragePutFileListener(gog_Interface listener){
  delete (_ICloudStoragePutFileListener*)listener;
}

class _ICloudStorageDeleteFileListener : public galaxy::api::ICloudStorageDeleteFileListener {
public:
  struct gog_listener listener;

  void OnDeleteFileFailure(const char* container, const char* name, galaxy::api::ICloudStorageDeleteFileListener::FailureReason failureReason) override {
    if(!this->listener.OnDeleteFileFailure) return;
    this->listener.OnDeleteFileFailure(this->listener.userptr, (const char*)container, (const char*)name, (enum gog_DeleteFileFailureReason)failureReason);
  }

  void OnDeleteFileSuccess(const char* container, const char* name) override {
    if(!this->listener.OnDeleteFileSuccess) return;
    this->listener.OnDeleteFileSuccess(this->listener.userptr, (const char*)container, (const char*)name);
  }
};

gog_Interface make_ICloudStorageDeleteFileListener(struct gog_listener *listener){
  _ICloudStorageDeleteFileListener *list = new _ICloudStorageDeleteFileListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ICloudStorageDeleteFileListener(gog_Interface listener){
  delete (_ICloudStorageDeleteFileListener*)listener;
}

class _IConnectionOpenListener : public galaxy::api::IConnectionOpenListener {
public:
  struct gog_listener listener;

  void OnConnectionOpenFailure(const char* connectionString, galaxy::api::IConnectionOpenListener::FailureReason failureReason) override {
    if(!this->listener.OnConnectionOpenFailure) return;
    this->listener.OnConnectionOpenFailure(this->listener.userptr, (const char*)connectionString, (enum gog_ConnectionOpenFailureReason)failureReason);
  }

  void OnConnectionOpenSuccess(const char* connectionString, ConnectionID connectionID) override {
    if(!this->listener.OnConnectionOpenSuccess) return;
    this->listener.OnConnectionOpenSuccess(this->listener.userptr, (const char*)connectionString, (gog_ConnectionID)connectionID);
  }
};

gog_Interface make_IConnectionOpenListener(struct gog_listener *listener){
  _IConnectionOpenListener *list = new _IConnectionOpenListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IConnectionOpenListener(gog_Interface listener){
  delete (_IConnectionOpenListener*)listener;
}

class _IConnectionCloseListener : public galaxy::api::IConnectionCloseListener {
public:
  struct gog_listener listener;

  void OnConnectionClosed(ConnectionID connectionID, CloseReason closeReason) override {
    if(!this->listener.OnConnectionClosed) return;
    this->listener.OnConnectionClosed(this->listener.userptr, (gog_ConnectionID)connectionID, (enum gog_CloseReason)closeReason);
  }
};

gog_Interface make_IConnectionCloseListener(struct gog_listener *listener){
  _IConnectionCloseListener *list = new _IConnectionCloseListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IConnectionCloseListener(gog_Interface listener){
  delete (_IConnectionCloseListener*)listener;
}

class _IConnectionDataListener : public galaxy::api::IConnectionDataListener {
public:
  struct gog_listener listener;

  void OnConnectionDataReceived(ConnectionID connectionID, uint32_t dataSize) override {
    if(!this->listener.OnConnectionDataReceived) return;
    this->listener.OnConnectionDataReceived(this->listener.userptr, (gog_ConnectionID)connectionID, (uint32_t)dataSize);
  }
};

gog_Interface make_IConnectionDataListener(struct gog_listener *listener){
  _IConnectionDataListener *list = new _IConnectionDataListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IConnectionDataListener(gog_Interface listener){
  delete (_IConnectionDataListener*)listener;
}

class _IPersonaDataChangedListener : public galaxy::api::IPersonaDataChangedListener {
public:
  struct gog_listener listener;

  void OnPersonaDataChanged(GalaxyID userID, uint32_t personaStateChange) override {
    if(!this->listener.OnPersonaDataChanged) return;
    this->listener.OnPersonaDataChanged(this->listener.userptr, (gog_ID)userID.ToUint64(), (uint32_t)personaStateChange);
  }
};

gog_Interface make_IPersonaDataChangedListener(struct gog_listener *listener){
  _IPersonaDataChangedListener *list = new _IPersonaDataChangedListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IPersonaDataChangedListener(gog_Interface listener){
  delete (_IPersonaDataChangedListener*)listener;
}

class _IUserInformationRetrieveListener : public galaxy::api::IUserInformationRetrieveListener {
public:
  struct gog_listener listener;

  void OnUserInformationRetrieveFailure(GalaxyID userID, galaxy::api::IUserInformationRetrieveListener::FailureReason failureReason) override {
    if(!this->listener.OnUserInformationRetrieveFailure) return;
    this->listener.OnUserInformationRetrieveFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FailureReason)failureReason);
  }

  void OnUserInformationRetrieveSuccess(GalaxyID userID) override {
    if(!this->listener.OnUserInformationRetrieveSuccess) return;
    this->listener.OnUserInformationRetrieveSuccess(this->listener.userptr, (gog_ID)userID.ToUint64());
  }
};

gog_Interface make_IUserInformationRetrieveListener(struct gog_listener *listener){
  _IUserInformationRetrieveListener *list = new _IUserInformationRetrieveListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IUserInformationRetrieveListener(gog_Interface listener){
  delete (_IUserInformationRetrieveListener*)listener;
}

class _IFriendListListener : public galaxy::api::IFriendListListener {
public:
  struct gog_listener listener;

  void OnFriendListRetrieveFailure(galaxy::api::IFriendListListener::FailureReason failureReason) override {
    if(!this->listener.OnFriendListRetrieveFailure) return;
    this->listener.OnFriendListRetrieveFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
  }

  void OnFriendListRetrieveSuccess() override {
    if(!this->listener.OnFriendListRetrieveSuccess) return;
    this->listener.OnFriendListRetrieveSuccess(this->listener.userptr);
  }
};

gog_Interface make_IFriendListListener(struct gog_listener *listener){
  _IFriendListListener *list = new _IFriendListListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IFriendListListener(gog_Interface listener){
  delete (_IFriendListListener*)listener;
}

class _IFriendInvitationSendListener : public galaxy::api::IFriendInvitationSendListener {
public:
  struct gog_listener listener;

  void OnFriendInvitationSendFailure(GalaxyID userID, galaxy::api::IFriendInvitationSendListener::FailureReason failureReason) override {
    if(!this->listener.OnFriendInvitationSendFailure) return;
    this->listener.OnFriendInvitationSendFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FriendInvitationFailureReason)failureReason);
  }

  void OnFriendInvitationSendSuccess(GalaxyID userID) override {
    if(!this->listener.OnFriendInvitationSendSuccess) return;
    this->listener.OnFriendInvitationSendSuccess(this->listener.userptr, (gog_ID)userID.ToUint64());
  }
};

gog_Interface make_IFriendInvitationSendListener(struct gog_listener *listener){
  _IFriendInvitationSendListener *list = new _IFriendInvitationSendListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IFriendInvitationSendListener(gog_Interface listener){
  delete (_IFriendInvitationSendListener*)listener;
}

class _IFriendInvitationListRetrieveListener : public galaxy::api::IFriendInvitationListRetrieveListener {
public:
  struct gog_listener listener;

  void OnFriendInvitationListRetrieveFailure(galaxy::api::IFriendInvitationListRetrieveListener::FailureReason failureReason) override {
    if(!this->listener.OnFriendInvitationListRetrieveFailure) return;
    this->listener.OnFriendInvitationListRetrieveFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
  }

  void OnFriendInvitationListRetrieveSuccess() override {
    if(!this->listener.OnFriendInvitationListRetrieveSuccess) return;
    this->listener.OnFriendInvitationListRetrieveSuccess(this->listener.userptr);
  }
};

gog_Interface make_IFriendInvitationListRetrieveListener(struct gog_listener *listener){
  _IFriendInvitationListRetrieveListener *list = new _IFriendInvitationListRetrieveListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IFriendInvitationListRetrieveListener(gog_Interface listener){
  delete (_IFriendInvitationListRetrieveListener*)listener;
}

class _ISentFriendInvitationListRetrieveListener : public galaxy::api::ISentFriendInvitationListRetrieveListener {
public:
  struct gog_listener listener;

  void OnSentFriendInvitationListRetrieveFailure(galaxy::api::ISentFriendInvitationListRetrieveListener::FailureReason failureReason) override {
    if(!this->listener.OnSentFriendInvitationListRetrieveFailure) return;
    this->listener.OnSentFriendInvitationListRetrieveFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
  }

  void OnSentFriendInvitationListRetrieveSuccess() override {
    if(!this->listener.OnSentFriendInvitationListRetrieveSuccess) return;
    this->listener.OnSentFriendInvitationListRetrieveSuccess(this->listener.userptr);
  }
};

gog_Interface make_ISentFriendInvitationListRetrieveListener(struct gog_listener *listener){
  _ISentFriendInvitationListRetrieveListener *list = new _ISentFriendInvitationListRetrieveListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ISentFriendInvitationListRetrieveListener(gog_Interface listener){
  delete (_ISentFriendInvitationListRetrieveListener*)listener;
}

class _IFriendInvitationListener : public galaxy::api::IFriendInvitationListener {
public:
  struct gog_listener listener;

  void OnFriendInvitationReceived(GalaxyID userID, uint32_t sendTime) override {
    if(!this->listener.OnFriendInvitationReceived) return;
    this->listener.OnFriendInvitationReceived(this->listener.userptr, (gog_ID)userID.ToUint64(), (uint32_t)sendTime);
  }
};

gog_Interface make_IFriendInvitationListener(struct gog_listener *listener){
  _IFriendInvitationListener *list = new _IFriendInvitationListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IFriendInvitationListener(gog_Interface listener){
  delete (_IFriendInvitationListener*)listener;
}

class _IFriendInvitationRespondToListener : public galaxy::api::IFriendInvitationRespondToListener {
public:
  struct gog_listener listener;

  void OnFriendInvitationRespondToFailure(GalaxyID userID, galaxy::api::IFriendInvitationRespondToListener::FailureReason failureReason) override {
    if(!this->listener.OnFriendInvitationRespondToFailure) return;
    this->listener.OnFriendInvitationRespondToFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FriendInvitationRespondFailureReason)failureReason);
  }

  void OnFriendInvitationRespondToSuccess(GalaxyID userID, bool accept) override {
    if(!this->listener.OnFriendInvitationRespondToSuccess) return;
    this->listener.OnFriendInvitationRespondToSuccess(this->listener.userptr, (gog_ID)userID.ToUint64(), (bool)accept);
  }
};

gog_Interface make_IFriendInvitationRespondToListener(struct gog_listener *listener){
  _IFriendInvitationRespondToListener *list = new _IFriendInvitationRespondToListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IFriendInvitationRespondToListener(gog_Interface listener){
  delete (_IFriendInvitationRespondToListener*)listener;
}

class _IFriendAddListener : public galaxy::api::IFriendAddListener {
public:
  struct gog_listener listener;

  void OnFriendAdded(GalaxyID userID, InvitationDirection invitationDirection) override {
    if(!this->listener.OnFriendAdded) return;
    this->listener.OnFriendAdded(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_InvitationDirection)invitationDirection);
  }
};

gog_Interface make_IFriendAddListener(struct gog_listener *listener){
  _IFriendAddListener *list = new _IFriendAddListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IFriendAddListener(gog_Interface listener){
  delete (_IFriendAddListener*)listener;
}

class _IFriendDeleteListener : public galaxy::api::IFriendDeleteListener {
public:
  struct gog_listener listener;

  void OnFriendDeleteFailure(GalaxyID userID, galaxy::api::IFriendDeleteListener::FailureReason failureReason) override {
    if(!this->listener.OnFriendDeleteFailure) return;
    this->listener.OnFriendDeleteFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FailureReason)failureReason);
  }

  void OnFriendDeleteSuccess(GalaxyID userID) override {
    if(!this->listener.OnFriendDeleteSuccess) return;
    this->listener.OnFriendDeleteSuccess(this->listener.userptr, (gog_ID)userID.ToUint64());
  }
};

gog_Interface make_IFriendDeleteListener(struct gog_listener *listener){
  _IFriendDeleteListener *list = new _IFriendDeleteListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IFriendDeleteListener(gog_Interface listener){
  delete (_IFriendDeleteListener*)listener;
}

class _IRichPresenceChangeListener : public galaxy::api::IRichPresenceChangeListener {
public:
  struct gog_listener listener;

  void OnRichPresenceChangeFailure(galaxy::api::IRichPresenceChangeListener::FailureReason failureReason) override {
    if(!this->listener.OnRichPresenceChangeFailure) return;
    this->listener.OnRichPresenceChangeFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
  }

  void OnRichPresenceChangeSuccess() override {
    if(!this->listener.OnRichPresenceChangeSuccess) return;
    this->listener.OnRichPresenceChangeSuccess(this->listener.userptr);
  }
};

gog_Interface make_IRichPresenceChangeListener(struct gog_listener *listener){
  _IRichPresenceChangeListener *list = new _IRichPresenceChangeListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IRichPresenceChangeListener(gog_Interface listener){
  delete (_IRichPresenceChangeListener*)listener;
}

class _IRichPresenceListener : public galaxy::api::IRichPresenceListener {
public:
  struct gog_listener listener;

  void OnRichPresenceUpdated(GalaxyID userID) override {
    if(!this->listener.OnRichPresenceUpdated) return;
    this->listener.OnRichPresenceUpdated(this->listener.userptr, (gog_ID)userID.ToUint64());
  }
};

gog_Interface make_IRichPresenceListener(struct gog_listener *listener){
  _IRichPresenceListener *list = new _IRichPresenceListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IRichPresenceListener(gog_Interface listener){
  delete (_IRichPresenceListener*)listener;
}

class _IRichPresenceRetrieveListener : public galaxy::api::IRichPresenceRetrieveListener {
public:
  struct gog_listener listener;

  void OnRichPresenceRetrieveFailure(GalaxyID userID, galaxy::api::IRichPresenceRetrieveListener::FailureReason failureReason) override {
    if(!this->listener.OnRichPresenceRetrieveFailure) return;
    this->listener.OnRichPresenceRetrieveFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FailureReason)failureReason);
  }

  void OnRichPresenceRetrieveSuccess(GalaxyID userID) override {
    if(!this->listener.OnRichPresenceRetrieveSuccess) return;
    this->listener.OnRichPresenceRetrieveSuccess(this->listener.userptr, (gog_ID)userID.ToUint64());
  }
};

gog_Interface make_IRichPresenceRetrieveListener(struct gog_listener *listener){
  _IRichPresenceRetrieveListener *list = new _IRichPresenceRetrieveListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IRichPresenceRetrieveListener(gog_Interface listener){
  delete (_IRichPresenceRetrieveListener*)listener;
}

class _IGameJoinRequestedListener : public galaxy::api::IGameJoinRequestedListener {
public:
  struct gog_listener listener;

  void OnGameJoinRequested(GalaxyID userID, const char* connectionString) override {
    if(!this->listener.OnGameJoinRequested) return;
    this->listener.OnGameJoinRequested(this->listener.userptr, (gog_ID)userID.ToUint64(), (const char*)connectionString);
  }
};

gog_Interface make_IGameJoinRequestedListener(struct gog_listener *listener){
  _IGameJoinRequestedListener *list = new _IGameJoinRequestedListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IGameJoinRequestedListener(gog_Interface listener){
  delete (_IGameJoinRequestedListener*)listener;
}

class _IGameInvitationReceivedListener : public galaxy::api::IGameInvitationReceivedListener {
public:
  struct gog_listener listener;

  void OnGameInvitationReceived(GalaxyID userID, const char* connectionString) override {
    if(!this->listener.OnGameInvitationReceived) return;
    this->listener.OnGameInvitationReceived(this->listener.userptr, (gog_ID)userID.ToUint64(), (const char*)connectionString);
  }
};

gog_Interface make_IGameInvitationReceivedListener(struct gog_listener *listener){
  _IGameInvitationReceivedListener *list = new _IGameInvitationReceivedListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IGameInvitationReceivedListener(gog_Interface listener){
  delete (_IGameInvitationReceivedListener*)listener;
}

class _ISendInvitationListener : public galaxy::api::ISendInvitationListener {
public:
  struct gog_listener listener;

  void OnInvitationSendFailure(GalaxyID userID, const char* connectionString, galaxy::api::ISendInvitationListener::FailureReason failureReason) override {
    if(!this->listener.OnInvitationSendFailure) return;
    this->listener.OnInvitationSendFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (const char*)connectionString, (enum gog_SendInvitationFailureReason)failureReason);
  }

  void OnInvitationSendSuccess(GalaxyID userID, const char* connectionString) override {
    if(!this->listener.OnInvitationSendSuccess) return;
    this->listener.OnInvitationSendSuccess(this->listener.userptr, (gog_ID)userID.ToUint64(), (const char*)connectionString);
  }
};

gog_Interface make_ISendInvitationListener(struct gog_listener *listener){
  _ISendInvitationListener *list = new _ISendInvitationListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ISendInvitationListener(gog_Interface listener){
  delete (_ISendInvitationListener*)listener;
}

class _IUserFindListener : public galaxy::api::IUserFindListener {
public:
  struct gog_listener listener;

  void OnUserFindFailure(const char* userSpecifier, galaxy::api::IUserFindListener::FailureReason failureReason) override {
    if(!this->listener.OnUserFindFailure) return;
    this->listener.OnUserFindFailure(this->listener.userptr, (const char*)userSpecifier, (enum gog_UserFindFailureReason)failureReason);
  }

  void OnUserFindSuccess(const char* userSpecifier, GalaxyID userID) override {
    if(!this->listener.OnUserFindSuccess) return;
    this->listener.OnUserFindSuccess(this->listener.userptr, (const char*)userSpecifier, (gog_ID)userID.ToUint64());
  }
};

gog_Interface make_IUserFindListener(struct gog_listener *listener){
  _IUserFindListener *list = new _IUserFindListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IUserFindListener(gog_Interface listener){
  delete (_IUserFindListener*)listener;
}

class _ILobbyListListener : public galaxy::api::ILobbyListListener {
public:
  struct gog_listener listener;

  void OnLobbyList(uint32_t lobbyCount, LobbyListResult result) override {
    if(!this->listener.OnLobbyList) return;
    this->listener.OnLobbyList(this->listener.userptr, (uint32_t)lobbyCount, (enum gog_LobbyListResult)result);
  }
};

gog_Interface make_ILobbyListListener(struct gog_listener *listener){
  _ILobbyListListener *list = new _ILobbyListListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILobbyListListener(gog_Interface listener){
  delete (_ILobbyListListener*)listener;
}

class _ILobbyCreatedListener : public galaxy::api::ILobbyCreatedListener {
public:
  struct gog_listener listener;

  void OnLobbyCreated(const GalaxyID& lobbyID, LobbyCreateResult result) override {
    if(!this->listener.OnLobbyCreated) return;
    this->listener.OnLobbyCreated(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (enum gog_LobbyCreateResult)result);
  }
};

gog_Interface make_ILobbyCreatedListener(struct gog_listener *listener){
  _ILobbyCreatedListener *list = new _ILobbyCreatedListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILobbyCreatedListener(gog_Interface listener){
  delete (_ILobbyCreatedListener*)listener;
}

class _ILobbyEnteredListener : public galaxy::api::ILobbyEnteredListener {
public:
  struct gog_listener listener;

  void OnLobbyEntered(const GalaxyID& lobbyID, LobbyEnterResult result) override {
    if(!this->listener.OnLobbyEntered) return;
    this->listener.OnLobbyEntered(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (enum gog_LobbyEnterResult)result);
  }
};

gog_Interface make_ILobbyEnteredListener(struct gog_listener *listener){
  _ILobbyEnteredListener *list = new _ILobbyEnteredListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILobbyEnteredListener(gog_Interface listener){
  delete (_ILobbyEnteredListener*)listener;
}

class _ILobbyLeftListener : public galaxy::api::ILobbyLeftListener {
public:
  struct gog_listener listener;

  void OnLobbyLeft(const GalaxyID& lobbyID, LobbyLeaveReason leaveReason) override {
    if(!this->listener.OnLobbyLeft) return;
    this->listener.OnLobbyLeft(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (enum gog_LobbyLeaveReason)leaveReason);
  }
};

gog_Interface make_ILobbyLeftListener(struct gog_listener *listener){
  _ILobbyLeftListener *list = new _ILobbyLeftListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILobbyLeftListener(gog_Interface listener){
  delete (_ILobbyLeftListener*)listener;
}

class _ILobbyDataListener : public galaxy::api::ILobbyDataListener {
public:
  struct gog_listener listener;

  void OnLobbyDataUpdated(const GalaxyID& lobbyID, const GalaxyID& memberID) override {
    if(!this->listener.OnLobbyDataUpdated) return;
    this->listener.OnLobbyDataUpdated(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (gog_ID)memberID.ToUint64());
  }
};

gog_Interface make_ILobbyDataListener(struct gog_listener *listener){
  _ILobbyDataListener *list = new _ILobbyDataListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILobbyDataListener(gog_Interface listener){
  delete (_ILobbyDataListener*)listener;
}

class _ILobbyDataUpdateListener : public galaxy::api::ILobbyDataUpdateListener {
public:
  struct gog_listener listener;

  void OnLobbyDataUpdateFailure(const GalaxyID& lobbyID, galaxy::api::ILobbyDataUpdateListener::FailureReason failureReason) override {
    if(!this->listener.OnLobbyDataUpdateFailure) return;
    this->listener.OnLobbyDataUpdateFailure(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (enum gog_LobbyDataUpdateFailureReason)failureReason);
  }

  void OnLobbyDataUpdateSuccess(const GalaxyID& lobbyID) override {
    if(!this->listener.OnLobbyDataUpdateSuccess) return;
    this->listener.OnLobbyDataUpdateSuccess(this->listener.userptr, (gog_ID)lobbyID.ToUint64());
  }
};

gog_Interface make_ILobbyDataUpdateListener(struct gog_listener *listener){
  _ILobbyDataUpdateListener *list = new _ILobbyDataUpdateListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILobbyDataUpdateListener(gog_Interface listener){
  delete (_ILobbyDataUpdateListener*)listener;
}

class _ILobbyMemberDataUpdateListener : public galaxy::api::ILobbyMemberDataUpdateListener {
public:
  struct gog_listener listener;

  void OnLobbyMemberDataUpdateFailure(const GalaxyID& lobbyID, const GalaxyID& memberID, galaxy::api::ILobbyMemberDataUpdateListener::FailureReason failureReason) override {
    if(!this->listener.OnLobbyMemberDataUpdateFailure) return;
    this->listener.OnLobbyMemberDataUpdateFailure(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (gog_ID)memberID.ToUint64(), (enum gog_LobbyDataUpdateFailureReason)failureReason);
  }

  void OnLobbyMemberDataUpdateSuccess(const GalaxyID& lobbyID, const GalaxyID& memberID) override {
    if(!this->listener.OnLobbyMemberDataUpdateSuccess) return;
    this->listener.OnLobbyMemberDataUpdateSuccess(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (gog_ID)memberID.ToUint64());
  }
};

gog_Interface make_ILobbyMemberDataUpdateListener(struct gog_listener *listener){
  _ILobbyMemberDataUpdateListener *list = new _ILobbyMemberDataUpdateListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILobbyMemberDataUpdateListener(gog_Interface listener){
  delete (_ILobbyMemberDataUpdateListener*)listener;
}

class _ILobbyDataRetrieveListener : public galaxy::api::ILobbyDataRetrieveListener {
public:
  struct gog_listener listener;

  void OnLobbyDataRetrieveFailure(const GalaxyID& lobbyID, galaxy::api::ILobbyDataRetrieveListener::FailureReason failureReason) override {
    if(!this->listener.OnLobbyDataRetrieveFailure) return;
    this->listener.OnLobbyDataRetrieveFailure(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (enum gog_LobbyDataRetrieveFailureReason)failureReason);
  }

  void OnLobbyDataRetrieveSuccess(const GalaxyID& lobbyID) override {
    if(!this->listener.OnLobbyDataRetrieveSuccess) return;
    this->listener.OnLobbyDataRetrieveSuccess(this->listener.userptr, (gog_ID)lobbyID.ToUint64());
  }
};

gog_Interface make_ILobbyDataRetrieveListener(struct gog_listener *listener){
  _ILobbyDataRetrieveListener *list = new _ILobbyDataRetrieveListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILobbyDataRetrieveListener(gog_Interface listener){
  delete (_ILobbyDataRetrieveListener*)listener;
}

class _ILobbyMemberStateListener : public galaxy::api::ILobbyMemberStateListener {
public:
  struct gog_listener listener;

  void OnLobbyMemberStateChanged(const GalaxyID& lobbyID, const GalaxyID& memberID, LobbyMemberStateChange memberStateChange) override {
    if(!this->listener.OnLobbyMemberStateChanged) return;
    this->listener.OnLobbyMemberStateChanged(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (gog_ID)memberID.ToUint64(), (enum gog_LobbyMemberStateChange)memberStateChange);
  }
};

gog_Interface make_ILobbyMemberStateListener(struct gog_listener *listener){
  _ILobbyMemberStateListener *list = new _ILobbyMemberStateListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILobbyMemberStateListener(gog_Interface listener){
  delete (_ILobbyMemberStateListener*)listener;
}

class _ILobbyOwnerChangeListener : public galaxy::api::ILobbyOwnerChangeListener {
public:
  struct gog_listener listener;

  void OnLobbyOwnerChanged(const GalaxyID& lobbyID, const GalaxyID& newOwnerID) override {
    if(!this->listener.OnLobbyOwnerChanged) return;
    this->listener.OnLobbyOwnerChanged(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (gog_ID)newOwnerID.ToUint64());
  }
};

gog_Interface make_ILobbyOwnerChangeListener(struct gog_listener *listener){
  _ILobbyOwnerChangeListener *list = new _ILobbyOwnerChangeListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILobbyOwnerChangeListener(gog_Interface listener){
  delete (_ILobbyOwnerChangeListener*)listener;
}

class _ILobbyMessageListener : public galaxy::api::ILobbyMessageListener {
public:
  struct gog_listener listener;

  void OnLobbyMessageReceived(const GalaxyID& lobbyID, const GalaxyID& senderID, uint32_t messageID, uint32_t messageLength) override {
    if(!this->listener.OnLobbyMessageReceived) return;
    this->listener.OnLobbyMessageReceived(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (gog_ID)senderID.ToUint64(), (uint32_t)messageID, (uint32_t)messageLength);
  }
};

gog_Interface make_ILobbyMessageListener(struct gog_listener *listener){
  _ILobbyMessageListener *list = new _ILobbyMessageListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILobbyMessageListener(gog_Interface listener){
  delete (_ILobbyMessageListener*)listener;
}

class _INetworkingListener : public galaxy::api::INetworkingListener {
public:
  struct gog_listener listener;

  void OnP2PPacketAvailable(uint32_t msgSize, uint8_t channel) override {
    if(!this->listener.OnP2PPacketAvailable) return;
    this->listener.OnP2PPacketAvailable(this->listener.userptr, (uint32_t)msgSize, (uint8_t)channel);
  }
};

gog_Interface make_INetworkingListener(struct gog_listener *listener){
  _INetworkingListener *list = new _INetworkingListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_INetworkingListener(gog_Interface listener){
  delete (_INetworkingListener*)listener;
}

class _INatTypeDetectionListener : public galaxy::api::INatTypeDetectionListener {
public:
  struct gog_listener listener;

  void OnNatTypeDetectionFailure() override {
    if(!this->listener.OnNatTypeDetectionFailure) return;
    this->listener.OnNatTypeDetectionFailure(this->listener.userptr);
  }

  void OnNatTypeDetectionSuccess(NatType natType) override {
    if(!this->listener.OnNatTypeDetectionSuccess) return;
    this->listener.OnNatTypeDetectionSuccess(this->listener.userptr, (enum gog_NatType)natType);
  }
};

gog_Interface make_INatTypeDetectionListener(struct gog_listener *listener){
  _INatTypeDetectionListener *list = new _INatTypeDetectionListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_INatTypeDetectionListener(gog_Interface listener){
  delete (_INatTypeDetectionListener*)listener;
}

class _IUserStatsAndAchievementsRetrieveListener : public galaxy::api::IUserStatsAndAchievementsRetrieveListener {
public:
  struct gog_listener listener;

  void OnUserStatsAndAchievementsRetrieveFailure(GalaxyID userID, galaxy::api::IUserStatsAndAchievementsRetrieveListener::FailureReason failureReason) override {
    if(!this->listener.OnUserStatsAndAchievementsRetrieveFailure) return;
    this->listener.OnUserStatsAndAchievementsRetrieveFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FailureReason)failureReason);
  }

  void OnUserStatsAndAchievementsRetrieveSuccess(GalaxyID userID) override {
    if(!this->listener.OnUserStatsAndAchievementsRetrieveSuccess) return;
    this->listener.OnUserStatsAndAchievementsRetrieveSuccess(this->listener.userptr, (gog_ID)userID.ToUint64());
  }
};

gog_Interface make_IUserStatsAndAchievementsRetrieveListener(struct gog_listener *listener){
  _IUserStatsAndAchievementsRetrieveListener *list = new _IUserStatsAndAchievementsRetrieveListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IUserStatsAndAchievementsRetrieveListener(gog_Interface listener){
  delete (_IUserStatsAndAchievementsRetrieveListener*)listener;
}

class _IStatsAndAchievementsStoreListener : public galaxy::api::IStatsAndAchievementsStoreListener {
public:
  struct gog_listener listener;

  void OnUserStatsAndAchievementsStoreFailure(galaxy::api::IStatsAndAchievementsStoreListener::FailureReason failureReason) override {
    if(!this->listener.OnUserStatsAndAchievementsStoreFailure) return;
    this->listener.OnUserStatsAndAchievementsStoreFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
  }

  void OnUserStatsAndAchievementsStoreSuccess() override {
    if(!this->listener.OnUserStatsAndAchievementsStoreSuccess) return;
    this->listener.OnUserStatsAndAchievementsStoreSuccess(this->listener.userptr);
  }
};

gog_Interface make_IStatsAndAchievementsStoreListener(struct gog_listener *listener){
  _IStatsAndAchievementsStoreListener *list = new _IStatsAndAchievementsStoreListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IStatsAndAchievementsStoreListener(gog_Interface listener){
  delete (_IStatsAndAchievementsStoreListener*)listener;
}

class _IAchievementChangeListener : public galaxy::api::IAchievementChangeListener {
public:
  struct gog_listener listener;

  void OnAchievementUnlocked(const char* name) override {
    if(!this->listener.OnAchievementUnlocked) return;
    this->listener.OnAchievementUnlocked(this->listener.userptr, (const char*)name);
  }
};

gog_Interface make_IAchievementChangeListener(struct gog_listener *listener){
  _IAchievementChangeListener *list = new _IAchievementChangeListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IAchievementChangeListener(gog_Interface listener){
  delete (_IAchievementChangeListener*)listener;
}

class _ILeaderboardsRetrieveListener : public galaxy::api::ILeaderboardsRetrieveListener {
public:
  struct gog_listener listener;

  void OnLeaderboardsRetrieveFailure(galaxy::api::ILeaderboardsRetrieveListener::FailureReason failureReason) override {
    if(!this->listener.OnLeaderboardsRetrieveFailure) return;
    this->listener.OnLeaderboardsRetrieveFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
  }

  void OnLeaderboardsRetrieveSuccess() override {
    if(!this->listener.OnLeaderboardsRetrieveSuccess) return;
    this->listener.OnLeaderboardsRetrieveSuccess(this->listener.userptr);
  }
};

gog_Interface make_ILeaderboardsRetrieveListener(struct gog_listener *listener){
  _ILeaderboardsRetrieveListener *list = new _ILeaderboardsRetrieveListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILeaderboardsRetrieveListener(gog_Interface listener){
  delete (_ILeaderboardsRetrieveListener*)listener;
}

class _ILeaderboardEntriesRetrieveListener : public galaxy::api::ILeaderboardEntriesRetrieveListener {
public:
  struct gog_listener listener;

  void OnLeaderboardEntriesRetrieveFailure(const char* name, galaxy::api::ILeaderboardEntriesRetrieveListener::FailureReason failureReason) override {
    if(!this->listener.OnLeaderboardEntriesRetrieveFailure) return;
    this->listener.OnLeaderboardEntriesRetrieveFailure(this->listener.userptr, (const char*)name, (enum gog_FailureReason)failureReason);
  }

  void OnLeaderboardEntriesRetrieveSuccess(const char* name, uint32_t entryCount) override {
    if(!this->listener.OnLeaderboardEntriesRetrieveSuccess) return;
    this->listener.OnLeaderboardEntriesRetrieveSuccess(this->listener.userptr, (const char*)name, (uint32_t)entryCount);
  }
};

gog_Interface make_ILeaderboardEntriesRetrieveListener(struct gog_listener *listener){
  _ILeaderboardEntriesRetrieveListener *list = new _ILeaderboardEntriesRetrieveListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILeaderboardEntriesRetrieveListener(gog_Interface listener){
  delete (_ILeaderboardEntriesRetrieveListener*)listener;
}

class _ILeaderboardScoreUpdateListener : public galaxy::api::ILeaderboardScoreUpdateListener {
public:
  struct gog_listener listener;

  void OnLeaderboardScoreUpdateFailure(const char* name, int32_t score, galaxy::api::ILeaderboardScoreUpdateListener::FailureReason failureReason) override {
    if(!this->listener.OnLeaderboardScoreUpdateFailure) return;
    this->listener.OnLeaderboardScoreUpdateFailure(this->listener.userptr, (const char*)name, (int32_t)score, (enum gog_LeaderboardScoreUpdateFailureReason)failureReason);
  }

  void OnLeaderboardScoreUpdateSuccess(const char* name, int32_t score, uint32_t oldRank, uint32_t newRank) override {
    if(!this->listener.OnLeaderboardScoreUpdateSuccess) return;
    this->listener.OnLeaderboardScoreUpdateSuccess(this->listener.userptr, (const char*)name, (int32_t)score, (uint32_t)oldRank, (uint32_t)newRank);
  }
};

gog_Interface make_ILeaderboardScoreUpdateListener(struct gog_listener *listener){
  _ILeaderboardScoreUpdateListener *list = new _ILeaderboardScoreUpdateListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILeaderboardScoreUpdateListener(gog_Interface listener){
  delete (_ILeaderboardScoreUpdateListener*)listener;
}

class _ILeaderboardRetrieveListener : public galaxy::api::ILeaderboardRetrieveListener {
public:
  struct gog_listener listener;

  void OnLeaderboardRetrieveFailure(const char* name, galaxy::api::ILeaderboardRetrieveListener::FailureReason failureReason) override {
    if(!this->listener.OnLeaderboardRetrieveFailure) return;
    this->listener.OnLeaderboardRetrieveFailure(this->listener.userptr, (const char*)name, (enum gog_FailureReason)failureReason);
  }

  void OnLeaderboardRetrieveSuccess(const char* name) override {
    if(!this->listener.OnLeaderboardRetrieveSuccess) return;
    this->listener.OnLeaderboardRetrieveSuccess(this->listener.userptr, (const char*)name);
  }
};

gog_Interface make_ILeaderboardRetrieveListener(struct gog_listener *listener){
  _ILeaderboardRetrieveListener *list = new _ILeaderboardRetrieveListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ILeaderboardRetrieveListener(gog_Interface listener){
  delete (_ILeaderboardRetrieveListener*)listener;
}

class _IUserTimePlayedRetrieveListener : public galaxy::api::IUserTimePlayedRetrieveListener {
public:
  struct gog_listener listener;

  void OnUserTimePlayedRetrieveFailure(GalaxyID userID, galaxy::api::IUserTimePlayedRetrieveListener::FailureReason failureReason) override {
    if(!this->listener.OnUserTimePlayedRetrieveFailure) return;
    this->listener.OnUserTimePlayedRetrieveFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FailureReason)failureReason);
  }

  void OnUserTimePlayedRetrieveSuccess(GalaxyID userID) override {
    if(!this->listener.OnUserTimePlayedRetrieveSuccess) return;
    this->listener.OnUserTimePlayedRetrieveSuccess(this->listener.userptr, (gog_ID)userID.ToUint64());
  }
};

gog_Interface make_IUserTimePlayedRetrieveListener(struct gog_listener *listener){
  _IUserTimePlayedRetrieveListener *list = new _IUserTimePlayedRetrieveListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IUserTimePlayedRetrieveListener(gog_Interface listener){
  delete (_IUserTimePlayedRetrieveListener*)listener;
}

class _IFileShareListener : public galaxy::api::IFileShareListener {
public:
  struct gog_listener listener;

  void OnFileShareFailure(const char* fileName, galaxy::api::IFileShareListener::FailureReason failureReason) override {
    if(!this->listener.OnFileShareFailure) return;
    this->listener.OnFileShareFailure(this->listener.userptr, (const char*)fileName, (enum gog_FailureReason)failureReason);
  }

  void OnFileShareSuccess(const char* fileName, SharedFileID sharedFileID) override {
    if(!this->listener.OnFileShareSuccess) return;
    this->listener.OnFileShareSuccess(this->listener.userptr, (const char*)fileName, (gog_SharedFileID)sharedFileID);
  }
};

gog_Interface make_IFileShareListener(struct gog_listener *listener){
  _IFileShareListener *list = new _IFileShareListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IFileShareListener(gog_Interface listener){
  delete (_IFileShareListener*)listener;
}

class _ISharedFileDownloadListener : public galaxy::api::ISharedFileDownloadListener {
public:
  struct gog_listener listener;

  void OnSharedFileDownloadFailure(SharedFileID sharedFileID, galaxy::api::ISharedFileDownloadListener::FailureReason failureReason) override {
    if(!this->listener.OnSharedFileDownloadFailure) return;
    this->listener.OnSharedFileDownloadFailure(this->listener.userptr, (gog_SharedFileID)sharedFileID, (enum gog_FailureReason)failureReason);
  }

  void OnSharedFileDownloadSuccess(SharedFileID sharedFileID, const char* fileName) override {
    if(!this->listener.OnSharedFileDownloadSuccess) return;
    this->listener.OnSharedFileDownloadSuccess(this->listener.userptr, (gog_SharedFileID)sharedFileID, (const char*)fileName);
  }
};

gog_Interface make_ISharedFileDownloadListener(struct gog_listener *listener){
  _ISharedFileDownloadListener *list = new _ISharedFileDownloadListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ISharedFileDownloadListener(gog_Interface listener){
  delete (_ISharedFileDownloadListener*)listener;
}

class _ITelemetryEventSendListener : public galaxy::api::ITelemetryEventSendListener {
public:
  struct gog_listener listener;

  void OnTelemetryEventSendFailure(const char* eventType, uint32_t sentEventIndex, galaxy::api::ITelemetryEventSendListener::FailureReason failureReason) override {
    if(!this->listener.OnTelemetryEventSendFailure) return;
    this->listener.OnTelemetryEventSendFailure(this->listener.userptr, (const char*)eventType, (uint32_t)sentEventIndex, (enum gog_TelemetryEventFailureReason)failureReason);
  }

  void OnTelemetryEventSendSuccess(const char* eventType, uint32_t sentEventIndex) override {
    if(!this->listener.OnTelemetryEventSendSuccess) return;
    this->listener.OnTelemetryEventSendSuccess(this->listener.userptr, (const char*)eventType, (uint32_t)sentEventIndex);
  }
};

gog_Interface make_ITelemetryEventSendListener(struct gog_listener *listener){
  _ITelemetryEventSendListener *list = new _ITelemetryEventSendListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ITelemetryEventSendListener(gog_Interface listener){
  delete (_ITelemetryEventSendListener*)listener;
}

class _IAuthListener : public galaxy::api::IAuthListener {
public:
  struct gog_listener listener;

  void OnAuthLost() override {
    if(!this->listener.OnAuthLost) return;
    this->listener.OnAuthLost(this->listener.userptr);
  }

  void OnAuthFailure(galaxy::api::IAuthListener::FailureReason failureReason) override {
    if(!this->listener.OnAuthFailure) return;
    this->listener.OnAuthFailure(this->listener.userptr, (enum gog_AuthFailureReason)failureReason);
  }

  void OnAuthSuccess() override {
    if(!this->listener.OnAuthSuccess) return;
    this->listener.OnAuthSuccess(this->listener.userptr);
  }
};

gog_Interface make_IAuthListener(struct gog_listener *listener){
  _IAuthListener *list = new _IAuthListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IAuthListener(gog_Interface listener){
  delete (_IAuthListener*)listener;
}

class _IOtherSessionStartListener : public galaxy::api::IOtherSessionStartListener {
public:
  struct gog_listener listener;

  void OnOtherSessionStarted() override {
    if(!this->listener.OnOtherSessionStarted) return;
    this->listener.OnOtherSessionStarted(this->listener.userptr);
  }
};

gog_Interface make_IOtherSessionStartListener(struct gog_listener *listener){
  _IOtherSessionStartListener *list = new _IOtherSessionStartListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IOtherSessionStartListener(gog_Interface listener){
  delete (_IOtherSessionStartListener*)listener;
}

class _IOperationalStateChangeListener : public galaxy::api::IOperationalStateChangeListener {
public:
  struct gog_listener listener;

  void OnOperationalStateChanged(uint32_t operationalState) override {
    if(!this->listener.OnOperationalStateChanged) return;
    this->listener.OnOperationalStateChanged(this->listener.userptr, (enum gog_OperationalState)operationalState);
  }
};

gog_Interface make_IOperationalStateChangeListener(struct gog_listener *listener){
  _IOperationalStateChangeListener *list = new _IOperationalStateChangeListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IOperationalStateChangeListener(gog_Interface listener){
  delete (_IOperationalStateChangeListener*)listener;
}

class _IUserDataListener : public galaxy::api::IUserDataListener {
public:
  struct gog_listener listener;

  void OnUserDataUpdated() override {
    if(!this->listener.OnUserDataUpdated) return;
    this->listener.OnUserDataUpdated(this->listener.userptr);
  }
};

gog_Interface make_IUserDataListener(struct gog_listener *listener){
  _IUserDataListener *list = new _IUserDataListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IUserDataListener(gog_Interface listener){
  delete (_IUserDataListener*)listener;
}

class _ISpecificUserDataListener : public galaxy::api::ISpecificUserDataListener {
public:
  struct gog_listener listener;

  void OnSpecificUserDataUpdated(GalaxyID userID) override {
    if(!this->listener.OnSpecificUserDataUpdated) return;
    this->listener.OnSpecificUserDataUpdated(this->listener.userptr, (gog_ID)userID.ToUint64());
  }
};

gog_Interface make_ISpecificUserDataListener(struct gog_listener *listener){
  _ISpecificUserDataListener *list = new _ISpecificUserDataListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_ISpecificUserDataListener(gog_Interface listener){
  delete (_ISpecificUserDataListener*)listener;
}

class _IEncryptedAppTicketListener : public galaxy::api::IEncryptedAppTicketListener {
public:
  struct gog_listener listener;

  void OnEncryptedAppTicketRetrieveFailure(galaxy::api::IEncryptedAppTicketListener::FailureReason failureReason) override {
    if(!this->listener.OnEncryptedAppTicketRetrieveFailure) return;
    this->listener.OnEncryptedAppTicketRetrieveFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
  }

  void OnEncryptedAppTicketRetrieveSuccess() override {
    if(!this->listener.OnEncryptedAppTicketRetrieveSuccess) return;
    this->listener.OnEncryptedAppTicketRetrieveSuccess(this->listener.userptr);
  }
};

gog_Interface make_IEncryptedAppTicketListener(struct gog_listener *listener){
  _IEncryptedAppTicketListener *list = new _IEncryptedAppTicketListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IEncryptedAppTicketListener(gog_Interface listener){
  delete (_IEncryptedAppTicketListener*)listener;
}

class _IAccessTokenListener : public galaxy::api::IAccessTokenListener {
public:
  struct gog_listener listener;

  void OnAccessTokenChanged() override {
    if(!this->listener.OnAccessTokenChanged) return;
    this->listener.OnAccessTokenChanged(this->listener.userptr);
  }
};

gog_Interface make_IAccessTokenListener(struct gog_listener *listener){
  _IAccessTokenListener *list = new _IAccessTokenListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IAccessTokenListener(gog_Interface listener){
  delete (_IAccessTokenListener*)listener;
}

class _IPlayFabCreateOpenIDConnectionListener : public galaxy::api::IPlayFabCreateOpenIDConnectionListener {
public:
  struct gog_listener listener;

  void OnPlayFabCreateOpenIDConnectionFailure(galaxy::api::IPlayFabCreateOpenIDConnectionListener::FailureReason failureReason) override {
    if(!this->listener.OnPlayFabCreateOpenIDConnectionFailure) return;
    this->listener.OnPlayFabCreateOpenIDConnectionFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
  }

  void OnPlayFabCreateOpenIDConnectionSuccess(bool connectionAlreadyExists) override {
    if(!this->listener.OnPlayFabCreateOpenIDConnectionSuccess) return;
    this->listener.OnPlayFabCreateOpenIDConnectionSuccess(this->listener.userptr, (bool)connectionAlreadyExists);
  }
};

gog_Interface make_IPlayFabCreateOpenIDConnectionListener(struct gog_listener *listener){
  _IPlayFabCreateOpenIDConnectionListener *list = new _IPlayFabCreateOpenIDConnectionListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IPlayFabCreateOpenIDConnectionListener(gog_Interface listener){
  delete (_IPlayFabCreateOpenIDConnectionListener*)listener;
}

class _IPlayFabLoginWithOpenIDConnectListener : public galaxy::api::IPlayFabLoginWithOpenIDConnectListener {
public:
  struct gog_listener listener;

  void OnPlayFabLoginWithOpenIDConnectFailure(galaxy::api::IPlayFabLoginWithOpenIDConnectListener::FailureReason failureReason) override {
    if(!this->listener.OnPlayFabLoginWithOpenIDConnectFailure) return;
    this->listener.OnPlayFabLoginWithOpenIDConnectFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
  }

  void OnPlayFabLoginWithOpenIDConnectSuccess() override {
    if(!this->listener.OnPlayFabLoginWithOpenIDConnectSuccess) return;
    this->listener.OnPlayFabLoginWithOpenIDConnectSuccess(this->listener.userptr);
  }
};

gog_Interface make_IPlayFabLoginWithOpenIDConnectListener(struct gog_listener *listener){
  _IPlayFabLoginWithOpenIDConnectListener *list = new _IPlayFabLoginWithOpenIDConnectListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IPlayFabLoginWithOpenIDConnectListener(gog_Interface listener){
  delete (_IPlayFabLoginWithOpenIDConnectListener*)listener;
}

class _IOverlayVisibilityChangeListener : public galaxy::api::IOverlayVisibilityChangeListener {
public:
  struct gog_listener listener;

  void OnOverlayVisibilityChanged(bool overlayVisible) override {
    if(!this->listener.OnOverlayVisibilityChanged) return;
    this->listener.OnOverlayVisibilityChanged(this->listener.userptr, (bool)overlayVisible);
  }
};

gog_Interface make_IOverlayVisibilityChangeListener(struct gog_listener *listener){
  _IOverlayVisibilityChangeListener *list = new _IOverlayVisibilityChangeListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IOverlayVisibilityChangeListener(gog_Interface listener){
  delete (_IOverlayVisibilityChangeListener*)listener;
}

class _IOverlayInitializationStateChangeListener : public galaxy::api::IOverlayInitializationStateChangeListener {
public:
  struct gog_listener listener;

  void OnOverlayStateChanged(OverlayState overlayState) override {
    if(!this->listener.OnOverlayStateChanged) return;
    this->listener.OnOverlayStateChanged(this->listener.userptr, (enum gog_OverlayState)overlayState);
  }
};

gog_Interface make_IOverlayInitializationStateChangeListener(struct gog_listener *listener){
  _IOverlayInitializationStateChangeListener *list = new _IOverlayInitializationStateChangeListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IOverlayInitializationStateChangeListener(gog_Interface listener){
  delete (_IOverlayInitializationStateChangeListener*)listener;
}

class _INotificationListener : public galaxy::api::INotificationListener {
public:
  struct gog_listener listener;

  void OnNotificationReceived(NotificationID notificationID, uint32_t typeLength, uint32_t contentSize) override {
    if(!this->listener.OnNotificationReceived) return;
    this->listener.OnNotificationReceived(this->listener.userptr, (gog_NotificationID)notificationID, (uint32_t)typeLength, (uint32_t)contentSize);
  }
};

gog_Interface make_INotificationListener(struct gog_listener *listener){
  _INotificationListener *list = new _INotificationListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_INotificationListener(gog_Interface listener){
  delete (_INotificationListener*)listener;
}

class _IGogServicesConnectionStateListener : public galaxy::api::IGogServicesConnectionStateListener {
public:
  struct gog_listener listener;

  void OnConnectionStateChange(GogServicesConnectionState connectionState) override {
    if(!this->listener.OnConnectionStateChange) return;
    this->listener.OnConnectionStateChange(this->listener.userptr, (enum gog_ServicesConnectionState)connectionState);
  }
};

gog_Interface make_IGogServicesConnectionStateListener(struct gog_listener *listener){
  _IGogServicesConnectionStateListener *list = new _IGogServicesConnectionStateListener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  if(list->listener.userptr == 0)
    list->listener.userptr = (void*)list;
  return (gog_Interface)list;
}

void free_IGogServicesConnectionStateListener(gog_Interface listener){
  delete (_IGogServicesConnectionStateListener*)listener;
}

struct __iface_init {
  __iface_init() {
    gog_listener_make[IS_DLC_OWNED] = make_IIsDlcOwnedListener;
    gog_listener_free[IS_DLC_OWNED] = free_IIsDlcOwnedListener;
    gog_listener_make[CHAT_ROOM_WITH_USER_RETRIEVE_LISTENER] = make_IChatRoomWithUserRetrieveListener;
    gog_listener_free[CHAT_ROOM_WITH_USER_RETRIEVE_LISTENER] = free_IChatRoomWithUserRetrieveListener;
    gog_listener_make[CHAT_ROOM_MESSAGE_SEND_LISTENER] = make_IChatRoomMessageSendListener;
    gog_listener_free[CHAT_ROOM_MESSAGE_SEND_LISTENER] = free_IChatRoomMessageSendListener;
    gog_listener_make[CHAT_ROOM_MESSAGES_LISTENER] = make_IChatRoomMessagesListener;
    gog_listener_free[CHAT_ROOM_MESSAGES_LISTENER] = free_IChatRoomMessagesListener;
    gog_listener_make[CHAT_ROOM_MESSAGES_RETRIEVE_LISTENER] = make_IChatRoomMessagesRetrieveListener;
    gog_listener_free[CHAT_ROOM_MESSAGES_RETRIEVE_LISTENER] = free_IChatRoomMessagesRetrieveListener;
    gog_listener_make[CLOUD_STORAGE_GET_FILE_LIST] = make_ICloudStorageGetFileListListener;
    gog_listener_free[CLOUD_STORAGE_GET_FILE_LIST] = free_ICloudStorageGetFileListListener;
    gog_listener_make[CLOUD_STORAGE_GET_FILE] = make_ICloudStorageGetFileListener;
    gog_listener_free[CLOUD_STORAGE_GET_FILE] = free_ICloudStorageGetFileListener;
    gog_listener_make[CLOUD_STORAGE_PUT_FILE] = make_ICloudStoragePutFileListener;
    gog_listener_free[CLOUD_STORAGE_PUT_FILE] = free_ICloudStoragePutFileListener;
    gog_listener_make[CLOUD_STORAGE_DELETE_FILE] = make_ICloudStorageDeleteFileListener;
    gog_listener_free[CLOUD_STORAGE_DELETE_FILE] = free_ICloudStorageDeleteFileListener;
    gog_listener_make[CUSTOM_NETWORKING_CONNECTION_OPEN] = make_IConnectionOpenListener;
    gog_listener_free[CUSTOM_NETWORKING_CONNECTION_OPEN] = free_IConnectionOpenListener;
    gog_listener_make[CUSTOM_NETWORKING_CONNECTION_CLOSE] = make_IConnectionCloseListener;
    gog_listener_free[CUSTOM_NETWORKING_CONNECTION_CLOSE] = free_IConnectionCloseListener;
    gog_listener_make[CUSTOM_NETWORKING_CONNECTION_DATA] = make_IConnectionDataListener;
    gog_listener_free[CUSTOM_NETWORKING_CONNECTION_DATA] = free_IConnectionDataListener;
    gog_listener_make[PERSONA_DATA_CHANGED] = make_IPersonaDataChangedListener;
    gog_listener_free[PERSONA_DATA_CHANGED] = free_IPersonaDataChangedListener;
    gog_listener_make[USER_INFORMATION_RETRIEVE_LISTENER] = make_IUserInformationRetrieveListener;
    gog_listener_free[USER_INFORMATION_RETRIEVE_LISTENER] = free_IUserInformationRetrieveListener;
    gog_listener_make[FRIEND_LIST_RETRIEVE] = make_IFriendListListener;
    gog_listener_free[FRIEND_LIST_RETRIEVE] = free_IFriendListListener;
    gog_listener_make[FRIEND_INVITATION_SEND_LISTENER] = make_IFriendInvitationSendListener;
    gog_listener_free[FRIEND_INVITATION_SEND_LISTENER] = free_IFriendInvitationSendListener;
    gog_listener_make[FRIEND_INVITATION_LIST_RETRIEVE_LISTENER] = make_IFriendInvitationListRetrieveListener;
    gog_listener_free[FRIEND_INVITATION_LIST_RETRIEVE_LISTENER] = free_IFriendInvitationListRetrieveListener;
    gog_listener_make[SENT_FRIEND_INVITATION_LIST_RETRIEVE_LISTENER] = make_ISentFriendInvitationListRetrieveListener;
    gog_listener_free[SENT_FRIEND_INVITATION_LIST_RETRIEVE_LISTENER] = free_ISentFriendInvitationListRetrieveListener;
    gog_listener_make[FRIEND_INVITATION_LISTENER] = make_IFriendInvitationListener;
    gog_listener_free[FRIEND_INVITATION_LISTENER] = free_IFriendInvitationListener;
    gog_listener_make[FRIEND_INVITATION_RESPOND_TO_LISTENER] = make_IFriendInvitationRespondToListener;
    gog_listener_free[FRIEND_INVITATION_RESPOND_TO_LISTENER] = free_IFriendInvitationRespondToListener;
    gog_listener_make[FRIEND_ADD_LISTENER] = make_IFriendAddListener;
    gog_listener_free[FRIEND_ADD_LISTENER] = free_IFriendAddListener;
    gog_listener_make[FRIEND_DELETE_LISTENER] = make_IFriendDeleteListener;
    gog_listener_free[FRIEND_DELETE_LISTENER] = free_IFriendDeleteListener;
    gog_listener_make[RICH_PRESENCE_CHANGE_LISTENER] = make_IRichPresenceChangeListener;
    gog_listener_free[RICH_PRESENCE_CHANGE_LISTENER] = free_IRichPresenceChangeListener;
    gog_listener_make[RICH_PRESENCE_LISTENER] = make_IRichPresenceListener;
    gog_listener_free[RICH_PRESENCE_LISTENER] = free_IRichPresenceListener;
    gog_listener_make[RICH_PRESENCE_RETRIEVE_LISTENER] = make_IRichPresenceRetrieveListener;
    gog_listener_free[RICH_PRESENCE_RETRIEVE_LISTENER] = free_IRichPresenceRetrieveListener;
    gog_listener_make[GAME_JOIN_REQUESTED_LISTENER] = make_IGameJoinRequestedListener;
    gog_listener_free[GAME_JOIN_REQUESTED_LISTENER] = free_IGameJoinRequestedListener;
    gog_listener_make[GAME_INVITATION_RECEIVED_LISTENER] = make_IGameInvitationReceivedListener;
    gog_listener_free[GAME_INVITATION_RECEIVED_LISTENER] = free_IGameInvitationReceivedListener;
    gog_listener_make[INVITATION_SEND] = make_ISendInvitationListener;
    gog_listener_free[INVITATION_SEND] = free_ISendInvitationListener;
    gog_listener_make[USER_FIND_LISTENER] = make_IUserFindListener;
    gog_listener_free[USER_FIND_LISTENER] = free_IUserFindListener;
    gog_listener_make[LOBBY_LIST] = make_ILobbyListListener;
    gog_listener_free[LOBBY_LIST] = free_ILobbyListListener;
    gog_listener_make[LOBBY_CREATED] = make_ILobbyCreatedListener;
    gog_listener_free[LOBBY_CREATED] = free_ILobbyCreatedListener;
    gog_listener_make[LOBBY_ENTERED] = make_ILobbyEnteredListener;
    gog_listener_free[LOBBY_ENTERED] = free_ILobbyEnteredListener;
    gog_listener_make[LOBBY_LEFT] = make_ILobbyLeftListener;
    gog_listener_free[LOBBY_LEFT] = free_ILobbyLeftListener;
    gog_listener_make[LOBBY_DATA] = make_ILobbyDataListener;
    gog_listener_free[LOBBY_DATA] = free_ILobbyDataListener;
    gog_listener_make[LOBBY_DATA_UPDATE_LISTENER] = make_ILobbyDataUpdateListener;
    gog_listener_free[LOBBY_DATA_UPDATE_LISTENER] = free_ILobbyDataUpdateListener;
    gog_listener_make[LOBBY_MEMBER_DATA_UPDATE_LISTENER] = make_ILobbyMemberDataUpdateListener;
    gog_listener_free[LOBBY_MEMBER_DATA_UPDATE_LISTENER] = free_ILobbyMemberDataUpdateListener;
    gog_listener_make[LOBBY_DATA_RETRIEVE] = make_ILobbyDataRetrieveListener;
    gog_listener_free[LOBBY_DATA_RETRIEVE] = free_ILobbyDataRetrieveListener;
    gog_listener_make[LOBBY_MEMBER_STATE] = make_ILobbyMemberStateListener;
    gog_listener_free[LOBBY_MEMBER_STATE] = free_ILobbyMemberStateListener;
    gog_listener_make[LOBBY_OWNER_CHANGE] = make_ILobbyOwnerChangeListener;
    gog_listener_free[LOBBY_OWNER_CHANGE] = free_ILobbyOwnerChangeListener;
    gog_listener_make[LOBBY_MESSAGE] = make_ILobbyMessageListener;
    gog_listener_free[LOBBY_MESSAGE] = free_ILobbyMessageListener;
    gog_listener_make[NETWORKING] = make_INetworkingListener;
    gog_listener_free[NETWORKING] = free_INetworkingListener;
    gog_listener_make[NAT_TYPE_DETECTION] = make_INatTypeDetectionListener;
    gog_listener_free[NAT_TYPE_DETECTION] = free_INatTypeDetectionListener;
    gog_listener_make[USER_STATS_AND_ACHIEVEMENTS_RETRIEVE] = make_IUserStatsAndAchievementsRetrieveListener;
    gog_listener_free[USER_STATS_AND_ACHIEVEMENTS_RETRIEVE] = free_IUserStatsAndAchievementsRetrieveListener;
    gog_listener_make[STATS_AND_ACHIEVEMENTS_STORE] = make_IStatsAndAchievementsStoreListener;
    gog_listener_free[STATS_AND_ACHIEVEMENTS_STORE] = free_IStatsAndAchievementsStoreListener;
    gog_listener_make[ACHIEVEMENT_CHANGE] = make_IAchievementChangeListener;
    gog_listener_free[ACHIEVEMENT_CHANGE] = free_IAchievementChangeListener;
    gog_listener_make[LEADERBOARDS_RETRIEVE] = make_ILeaderboardsRetrieveListener;
    gog_listener_free[LEADERBOARDS_RETRIEVE] = free_ILeaderboardsRetrieveListener;
    gog_listener_make[LEADERBOARD_ENTRIES_RETRIEVE] = make_ILeaderboardEntriesRetrieveListener;
    gog_listener_free[LEADERBOARD_ENTRIES_RETRIEVE] = free_ILeaderboardEntriesRetrieveListener;
    gog_listener_make[LEADERBOARD_SCORE_UPDATE_LISTENER] = make_ILeaderboardScoreUpdateListener;
    gog_listener_free[LEADERBOARD_SCORE_UPDATE_LISTENER] = free_ILeaderboardScoreUpdateListener;
    gog_listener_make[LEADERBOARD_RETRIEVE] = make_ILeaderboardRetrieveListener;
    gog_listener_free[LEADERBOARD_RETRIEVE] = free_ILeaderboardRetrieveListener;
    gog_listener_make[USER_TIME_PLAYED_RETRIEVE] = make_IUserTimePlayedRetrieveListener;
    gog_listener_free[USER_TIME_PLAYED_RETRIEVE] = free_IUserTimePlayedRetrieveListener;
    gog_listener_make[FILE_SHARE] = make_IFileShareListener;
    gog_listener_free[FILE_SHARE] = free_IFileShareListener;
    gog_listener_make[SHARED_FILE_DOWNLOAD] = make_ISharedFileDownloadListener;
    gog_listener_free[SHARED_FILE_DOWNLOAD] = free_ISharedFileDownloadListener;
    gog_listener_make[TELEMETRY_EVENT_SEND_LISTENER] = make_ITelemetryEventSendListener;
    gog_listener_free[TELEMETRY_EVENT_SEND_LISTENER] = free_ITelemetryEventSendListener;
    gog_listener_make[AUTH] = make_IAuthListener;
    gog_listener_free[AUTH] = free_IAuthListener;
    gog_listener_make[OTHER_SESSION_START] = make_IOtherSessionStartListener;
    gog_listener_free[OTHER_SESSION_START] = free_IOtherSessionStartListener;
    gog_listener_make[OPERATIONAL_STATE_CHANGE] = make_IOperationalStateChangeListener;
    gog_listener_free[OPERATIONAL_STATE_CHANGE] = free_IOperationalStateChangeListener;
    gog_listener_make[USER_DATA] = make_IUserDataListener;
    gog_listener_free[USER_DATA] = free_IUserDataListener;
    gog_listener_make[SPECIFIC_USER_DATA] = make_ISpecificUserDataListener;
    gog_listener_free[SPECIFIC_USER_DATA] = free_ISpecificUserDataListener;
    gog_listener_make[ENCRYPTED_APP_TICKET_RETRIEVE] = make_IEncryptedAppTicketListener;
    gog_listener_free[ENCRYPTED_APP_TICKET_RETRIEVE] = free_IEncryptedAppTicketListener;
    gog_listener_make[ACCESS_TOKEN_CHANGE] = make_IAccessTokenListener;
    gog_listener_free[ACCESS_TOKEN_CHANGE] = free_IAccessTokenListener;
    gog_listener_make[PLAYFAB_CREATE_OPENID_CONNECTION] = make_IPlayFabCreateOpenIDConnectionListener;
    gog_listener_free[PLAYFAB_CREATE_OPENID_CONNECTION] = free_IPlayFabCreateOpenIDConnectionListener;
    gog_listener_make[PLAYFAB_LOGIN_WITH_OPENID_CONNECT] = make_IPlayFabLoginWithOpenIDConnectListener;
    gog_listener_free[PLAYFAB_LOGIN_WITH_OPENID_CONNECT] = free_IPlayFabLoginWithOpenIDConnectListener;
    gog_listener_make[OVERLAY_VISIBILITY_CHANGE] = make_IOverlayVisibilityChangeListener;
    gog_listener_free[OVERLAY_VISIBILITY_CHANGE] = free_IOverlayVisibilityChangeListener;
    gog_listener_make[OVERLAY_INITIALIZATION_STATE_CHANGE] = make_IOverlayInitializationStateChangeListener;
    gog_listener_free[OVERLAY_INITIALIZATION_STATE_CHANGE] = free_IOverlayInitializationStateChangeListener;
    gog_listener_make[NOTIFICATION_LISTENER] = make_INotificationListener;
    gog_listener_free[NOTIFICATION_LISTENER] = free_INotificationListener;
    gog_listener_make[GOG_SERVICES_CONNECTION_STATE_LISTENER] = make_IGogServicesConnectionStateListener;
    gog_listener_free[GOG_SERVICES_CONNECTION_STATE_LISTENER] = free_IGogServicesConnectionStateListener;
    
  }
};
__iface_init __iface_init_;