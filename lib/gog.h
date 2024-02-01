#include <stdint.h>
#include <stdbool.h>

//// GalaxyAllocator.h
typedef void* (*gog_GalaxyMalloc)(uint32_t size, char* typeName);
typedef void* (*gog_GalaxyRealloc)(void* ptr, uint32_t newSize, char* typeName);
typedef void (*gog_GalaxyFree)(void* ptr);
void *gog_MakeAllocator(gog_GalaxyMalloc malloc, gog_GalaxyRealloc realloc, gog_GalaxyFree free);
void gog_FreeAllocator(void *allocator);

//// GalaxyThread.h
struct gog_Thread{
  void (*join)();
  bool (*joinable)();
  void (*detach)();
  void *user;
};

typedef void (*gog_ThreadEntryFunction)(void *user);
typedef struct gog_Thread* (*gog_ThreadCreationFunction)(gog_ThreadEntryFunction, void *user);
void *gog_MakeThreadFactory(gog_ThreadCreationFunction creator);
void gog_FreeThreadFactor(void *factory);

//// GalaxyID.h
enum gog_ID_Type{
  ID_TYPE_UNASSIGNED,
  ID_TYPE_LOBBY,
  ID_TYPE_USER
};

typedef uint64_t gog_ID;

inline gog_ID gog_ID_FromRealID(enum gog_ID_Type type, uint64_t value){
  return (uint64_t)type << 56 | value;
}

inline uint64_t gog_ID_GetRealID(gog_ID id){
  return id & 0xFFFFFFFFFFFFFF;
}

inline enum gog_ID_Type gog_ID_GetType(gog_ID id){
  return id >> 56;
}

inline bool gog_ID_IsValid(gog_ID id){
  return id != 0;
}

//// InitOptions.h
struct gog_InitOptions {
  char *client_id;
  char *client_secret;
  char *config_file_path;
  char *storage_path;
  void *galaxy_allocator;
  void *galaxy_thread_factory;
  char *host;
  uint16_t port;
};

//// ShutdownOptions.h
struct gog_ShutdownOptions {
  bool preserve_static_objects;
};

//// GalaxyApi.h
typedef void* gog_Interface;

void gog_Init(struct gog_InitOptions *options);
void gog_Shutdown();
void gog_ShutdownEx(struct gog_ShutdownOptions *options);
gog_Interface gog_User();
gog_Interface gog_Friends();
gog_Interface gog_Chat();
gog_Interface gog_Matchmaking();
gog_Interface gog_Networking();
gog_Interface gog_Stats();
gog_Interface gog_Utils();
gog_Interface gog_Apps();
gog_Interface gog_Storage();
gog_Interface gog_CustomNetworking();
gog_Interface gog_Logger();
gog_Interface gog_Telemetry();
gog_Interface gog_CloudStorage();
void gog_ProcessData();

//// Errors.h
enum gog_ErrorType {
  GOG_UNAUTHORIZED_ACCESS,
  GOG_INVALID_ARGUMENT,
  GOG_INVALID_STATE,
  GOG_RUNTIME_ERROR
};

char* gog_IError_GetName(gog_Interface error);
char* gog_IError_GetMsg(gog_Interface error);
enum gog_ErrorType gog_IError_GetType(gog_Interface error);
gog_Interface gog_GetError();

//// IListenerRegistrar.h
enum gog_ListenerType {
  GOG_LISTENER_TYPE_BEGIN, 
  GOG_LOBBY_LIST = GOG_LISTENER_TYPE_BEGIN, 
  GOG_LOBBY_CREATED, 
  GOG_LOBBY_ENTERED, 
  GOG_LOBBY_LEFT, 
  GOG_LOBBY_DATA, 
  GOG_LOBBY_MEMBER_STATE, 
  GOG_LOBBY_OWNER_CHANGE, 
  GOG_AUTH, 
  GOG_LOBBY_MESSAGE, 
  GOG_NETWORKING, 
  GOG_USER_DATA, 
  GOG_USER_STATS_AND_ACHIEVEMENTS_RETRIEVE, 
  GOG_STATS_AND_ACHIEVEMENTS_STORE, 
  GOG_ACHIEVEMENT_CHANGE, 
  GOG_LEADERBOARDS_RETRIEVE, 
  GOG_LEADERBOARD_ENTRIES_RETRIEVE, 
  GOG_LEADERBOARD_SCORE_UPDATE_LISTENER, 
  GOG_PERSONA_DATA_CHANGED, 
  GOG_RICH_PRESENCE_CHANGE_LISTENER, 
  GOG_GAME_JOIN_REQUESTED_LISTENER, 
  GOG_OPERATIONAL_STATE_CHANGE, 
  GOG_FRIEND_LIST_RETRIEVE, 
  GOG_ENCRYPTED_APP_TICKET_RETRIEVE, 
  GOG_ACCESS_TOKEN_CHANGE, 
  GOG_LEADERBOARD_RETRIEVE, 
  GOG_SPECIFIC_USER_DATA, 
  GOG_INVITATION_SEND, 
  GOG_RICH_PRESENCE_LISTENER, 
  GOG_GAME_INVITATION_RECEIVED_LISTENER, 
  GOG_NOTIFICATION_LISTENER, 
  GOG_LOBBY_DATA_RETRIEVE, 
  GOG_USER_TIME_PLAYED_RETRIEVE, 
  GOG_OTHER_SESSION_START, 
  GOG_FILE_SHARE, 
  GOG_SHARED_FILE_DOWNLOAD, 
  GOG_CUSTOM_NETWORKING_CONNECTION_OPEN, 
  GOG_CUSTOM_NETWORKING_CONNECTION_CLOSE, 
  GOG_CUSTOM_NETWORKING_CONNECTION_DATA, 
  GOG_OVERLAY_INITIALIZATION_STATE_CHANGE, 
  GOG_OVERLAY_VISIBILITY_CHANGE, 
  GOG_CHAT_ROOM_WITH_USER_RETRIEVE_LISTENER, 
  GOG_CHAT_ROOM_MESSAGE_SEND_LISTENER, 
  GOG_CHAT_ROOM_MESSAGES_LISTENER, 
  GOG_FRIEND_INVITATION_SEND_LISTENER, 
  GOG_FRIEND_INVITATION_LIST_RETRIEVE_LISTENER, 
  GOG_FRIEND_INVITATION_LISTENER, 
  GOG_FRIEND_INVITATION_RESPOND_TO_LISTENER, 
  GOG_FRIEND_ADD_LISTENER, 
  GOG_FRIEND_DELETE_LISTENER, 
  GOG_CHAT_ROOM_MESSAGES_RETRIEVE_LISTENER, 
  GOG_USER_FIND_LISTENER, 
  GOG_NAT_TYPE_DETECTION, 
  GOG_SENT_FRIEND_INVITATION_LIST_RETRIEVE_LISTENER, 
  GOG_LOBBY_DATA_UPDATE_LISTENER, /// < Used by ILobbyDataUpdateListener.
  GOG_LOBBY_MEMBER_DATA_UPDATE_LISTENER, /// < Used by ILobbyDataUpdateListener.
  GOG_USER_INFORMATION_RETRIEVE_LISTENER, 
  GOG_RICH_PRESENCE_RETRIEVE_LISTENER, 
  GOG_GOG_SERVICES_CONNECTION_STATE_LISTENER, 
  GOG_TELEMETRY_EVENT_SEND_LISTENER, 
  GOG_CLOUD_STORAGE_GET_FILE_LIST, 
  GOG_CLOUD_STORAGE_GET_FILE, 
  GOG_CLOUD_STORAGE_PUT_FILE, 
  GOG_CLOUD_STORAGE_DELETE_FILE, 
  GOG_IS_DLC_OWNED, 
  GOG_PLAYFAB_CREATE_OPENID_CONNECTION, 
  GOG_PLAYFAB_LOGIN_WITH_OPENID_CONNECT, 
  GOG_LISTENER_TYPE_END 
};

void gog_IListenerRegistrar_Register(gog_Interface registrar, enum gog_ListenerType type, void *listener);
void gog_IListenerRegistrar_Unregister(gog_Interface registrar, enum gog_ListenerType type, void *listener);
gog_Interface gog_ListenerRegistrar();
gog_Interface gog_GameServerListenerRegistrar();

enum gog_BasicFailureReason {
  GOG_BASIC_FAILURE_REASON_UNDEFINED, 
  GOG_BASIC_FAILURE_REASON_CONNECTION_FAILURE 
};

// IEncryptedAppTicketListener
// IPlayFabCreateOpenIDConnectionListener
// IPlayFabLoginWithOpenIDConnectListener
// IUserInformationRetrieveListener
// IFriendListListener
// IFriendInvitationListRetrieveListener
// ISentFriendInvitationListRetrieveListener
// IFriendDeleteListener
// IRichPresenceChangeListener
// IRichPresenceRetrieveListener
struct gog_BasicListener {
  void (*OnSuccess)();
  void (*OnFailure)(enum gog_BasicFailureReason reason);
};

//// IUser.h
typedef uint64_t gog_SessionID;

enum gog_AuthFailureReason {
  GOG_FAILURE_REASON_UNDEFINED, 
  GOG_FAILURE_REASON_GALAXY_SERVICE_NOT_AVAILABLE, 
  GOG_FAILURE_REASON_GALAXY_SERVICE_NOT_SIGNED_IN, 
  GOG_FAILURE_REASON_CONNECTION_FAILURE, 
  GOG_FAILURE_REASON_NO_LICENSE, 
  GOG_FAILURE_REASON_INVALID_CREDENTIALS, 
  GOG_FAILURE_REASON_GALAXY_NOT_INITIALIZED, 
  GOG_FAILURE_REASON_EXTERNAL_SERVICE_FAILURE 
};

struct gog_IAuthListener {
  void (*OnAuthSuccess)();
  void (*OnAuthFailure)(enum gog_AuthFailureReason reason);
  void (*OnAuthLost)();
};

struct gog_IOtherSessionStartListener {
  void (*OnOtherSessionStarted)();
};

enum gog_OperationalState {
  OPERATIONAL_STATE_SIGNED_IN = 0x0001, 
  OPERATIONAL_STATE_LOGGED_ON = 0x0002 
};

struct gog_IOperationalStateChangeListener {
  void (*OnOperationalStateChanged)(enum gog_OperationalState state);
};

struct gog_IUserDataListener {
  void (*OnUserDataUpdated)();
};

struct gog_ISpecificUserDataListener {
  void (*OnSpecificUserDataUpdated)(gog_ID user);
};

struct gog_IAccessTokenListener {
  void (*OnAccessTokenChanged)();
};

bool gog_IUser_SignedIn(gog_Interface user);
gog_ID gog_IUser_GetGalaxyID(gog_Interface user);
void gog_IUser_SignInCredentials(gog_Interface user, char* login, char* password, gog_Interface listener);
void gog_IUser_SignInToken(gog_Interface user, char* refreshToken, gog_Interface listener);
void gog_IUser_SignInLauncher(gog_Interface user, gog_Interface listener);
void gog_IUser_SignInSteam(gog_Interface user, void* steamAppTicket, uint32_t steamAppTicketSize, char* personaName, gog_Interface listener);
void gog_IUser_SignInGalaxy(gog_Interface user, bool requireOnline, uint32_t timeout, gog_Interface listener);
void gog_IUser_SignInPS4(gog_Interface user, char* ps4ClientID, gog_Interface listener);
void gog_IUser_SignInXB1(gog_Interface user, char* xboxOneUserID, gog_Interface listener);
void gog_IUser_SignInXbox(gog_Interface user, uint64_t xboxID, gog_Interface listener);
void gog_IUser_SignInXBLive(gog_Interface user, char* token, char* signature, char* marketplaceID, char* locale, gog_Interface listener);
void gog_IUser_SignInAnonymous(gog_Interface user, gog_Interface listener);
void gog_IUser_SignInAnonymousTelemetry(gog_Interface user, gog_Interface listener);
void gog_IUser_SignInServerKey(gog_Interface user, char* serverKey, gog_Interface listener);
void gog_IUser_SignInAuthorizationCode(gog_Interface user, char* authorizationCode, char* redirectURI, gog_Interface listener);
void gog_IUser_SignOut(gog_Interface user);
void gog_IUser_RequestUserData(gog_Interface user, gog_ID userID, gog_Interface listener);
bool gog_IUser_IsUserDataAvailable(gog_Interface user, gog_ID userID);
char* gog_IUser_GetUserData(gog_Interface user, char* key, gog_ID userID);
void gog_IUser_GetUserDataCopy(gog_Interface user, char* key, char* buffer, uint32_t bufferLength, gog_ID userID);
void gog_IUser_SetUserData(gog_Interface user, char* key, char* value, gog_Interface listener);
uint32_t gog_IUser_GetUserDataCount(gog_Interface user, gog_ID userID);
bool gog_IUser_GetUserDataByIndex(gog_Interface user, uint32_t index, char* key, uint32_t keyLength, char* value, uint32_t valueLength, gog_ID userID);
void gog_IUser_DeleteUserData(gog_Interface user, char* key, gog_Interface listener);
bool gog_IUser_IsLoggedOn(gog_Interface user);
void gog_IUser_RequestEncryptedAppTicket(gog_Interface user, void* data, uint32_t dataSize, gog_Interface listener);
void gog_IUser_GetEncryptedAppTicket(gog_Interface user, void* encryptedAppTicket, uint32_t maxEncryptedAppTicketSize, uint32_t* currentEncryptedAppTicketSize);
void gog_IUser_CreateOpenIDConnection(gog_Interface user, char* secretKey, char* titleID, char* connectionID, bool ignoreNonce, gog_Interface listener);
void gog_IUser_LoginWithOpenIDConnect(gog_Interface user, char* titleID, char* connectionID, char* idToken, bool createAccount, char* encryptedRequest, char* playerSecret, gog_Interface listener);
gog_SessionID gog_IUser_GetSessionID(gog_Interface user);
char* gog_IUser_GetAccessToken(gog_Interface user);
void gog_IUser_GetAccessTokenCopy(gog_Interface user, char* buffer, uint32_t bufferLength);
char* gog_IUser_GetRefreshToken(gog_Interface user);
void gog_IUser_GetRefreshTokenCopy(gog_Interface user, char* buffer, uint32_t bufferLength);
char* gog_IUser_GetIDToken(gog_Interface user);
void gog_IUser_GetIDTokenCopy(gog_Interface user, char* buffer, uint32_t bufferLength);
bool gog_IUser_ReportInvalidAccessToken(gog_Interface user, char* accessToken, char* info);

//// IFriends.h
enum gog_AvatarType {
  GOG_AVATAR_TYPE_NONE = 0x0000,
  GOG_AVATAR_TYPE_SMALL = 0x0001,
  GOG_AVATAR_TYPE_MEDIUM = 0x0002,
  GOG_AVATAR_TYPE_LARGE = 0x0004
};

enum gog_PersonaState {
  GOG_PERSONA_STATE_OFFLINE,
  GOG_PERSONA_STATE_ONLINE
};

typedef uint32_t gog_AvatarCriteria;

enum gog_PersonaStateChange {
  GOG_PERSONA_CHANGE_NONE = 0x0000,
  GOG_PERSONA_CHANGE_NAME = 0x0001,
  GOG_PERSONA_CHANGE_AVATAR = 0x0002,
  GOG_PERSONA_CHANGE_AVATAR_DOWNLOADED_IMAGE_SMALL = 0x0004,
  GOG_PERSONA_CHANGE_AVATAR_DOWNLOADED_IMAGE_MEDIUM = 0x0008,
  GOG_PERSONA_CHANGE_AVATAR_DOWNLOADED_IMAGE_LARGE = 0x0010,
  GOG_PERSONA_CHANGE_AVATAR_DOWNLOADED_IMAGE_ANY = GOG_PERSONA_CHANGE_AVATAR_DOWNLOADED_IMAGE_SMALL | GOG_PERSONA_CHANGE_AVATAR_DOWNLOADED_IMAGE_MEDIUM | GOG_PERSONA_CHANGE_AVATAR_DOWNLOADED_IMAGE_LARGE
};

struct gog_IPersonaDataChangedListener {
  void (*OnPersonaDataChanged)(gog_ID userID, uint32_t personaStateChange);
};

enum gog_FriendInvitationFailureReason {
  GOG_FRIENDINVITATION_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_FRIENDINVITATION_FAILURE_REASON_USER_DOES_NOT_EXIST, ///< User does not exist.
  GOG_FRIENDINVITATION_FAILURE_REASON_USER_ALREADY_INVITED, ///< Friend invitation already sent to the user.
  GOG_FRIENDINVITATION_FAILURE_REASON_USER_ALREADY_FRIEND, ///< User already on the friend list.
  GOG_FRIENDINVITATION_FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

struct gog_IFriendInvitationSendListener {
  void (*OnFriendInvitationSendSuccess)(gog_ID userID);
  void (*OnFriendInvitationSendFailure)(gog_ID userID, enum gog_FriendInvitationFailureReason failureReason);
};

struct gog_IFriendInvitationListener {
  void (*OnFriendInvitationReceived)(gog_ID userID, uint32_t sendTime);
};

enum gog_FriendInvitationRespondFailureReason {
  GOG_FRIENDINVITATIONRESPOND_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_FRIENDINVITATIONRESPOND_FAILURE_REASON_USER_DOES_NOT_EXIST, ///< User does not exist.
  GOG_FRIENDINVITATIONRESPOND_FAILURE_REASON_FRIEND_INVITATION_DOES_NOT_EXIST, ///< Friend invitation does not exist.
  GOG_FRIENDINVITATIONRESPOND_FAILURE_REASON_USER_ALREADY_FRIEND, ///< User already on the friend list.
  GOG_FRIENDINVITATIONRESPOND_FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

struct gog_IFriendInvitationRespondToListener {
  void (*OnFriendInvitationRespondToSuccess)(gog_ID userID, bool accept);
  void (*OnFriendInvitationRespondToFailure)(gog_ID userID, enum gog_FriendInvitationRespondFailureReason failureReason);
};

enum gog_InvitationDirection{
  GOG_INVITATION_DIRECTION_INCOMING, ///< The user indicated in the notification was the inviter.
  GOG_INVITATION_DIRECTION_OUTGOING ///< The user indicated in the notification was the invitee.
};

struct gog_IFriendAddListener {
  void (*OnFriendAdded)(gog_ID userID, enum gog_InvitationDirection invitationDirection);
};

struct gog_IRichPresenceListener {
  void (*OnRichPresenceUpdated)(gog_ID userID);
};

struct gog_IGameJoinRequestedListener {
  void (*OnGameJoinRequested)(gog_ID userID, const char* connectionString);
};

struct gog_IGameInvitationReceivedListener {
  void (*OnGameInvitationReceived)(gog_ID userID, const char* connectionString);
};

enum gog_SendInvitationFailureReason{
  GOG_SENDINVITATION_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_SENDINVITATION_FAILURE_REASON_USER_DOES_NOT_EXIST, ///< Receiver does not exist.
  GOG_SENDINVITATION_FAILURE_REASON_RECEIVER_DOES_NOT_ALLOW_INVITING, ///< Receiver does not allow inviting
  GOG_SENDINVITATION_FAILURE_REASON_SENDER_DOES_NOT_ALLOW_INVITING, ///< Sender does not allow inviting
  GOG_SENDINVITATION_FAILURE_REASON_RECEIVER_BLOCKED, ///< Receiver blocked by sender.
  GOG_SENDINVITATION_FAILURE_REASON_SENDER_BLOCKED, ///< Sender blocked by receiver. Will also occur if both users blocked each other.
  GOG_SENDINVITATION_FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

struct gog_ISendInvitationListener {
  void (*OnInvitationSendSuccess)(gog_ID userID, const char* connectionString);
  void (*OnInvitationSendFailure)(gog_ID userID, const char* connectionString, enum gog_SendInvitationFailureReason failureReason);
};

enum gog_UserFindFailureReason {
  GOG_USERFIND_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_USERFIND_FAILURE_REASON_USER_NOT_FOUND, ///< Specified user was not found.
  GOG_USERFIND_FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

struct gog_IUserFindListener {
  void (*OnUserFindSuccess)(const char* userSpecifier, gog_ID userID);
  void (*OnUserFindFailure)(const char* userSpecifier, enum gog_UserFindFailureReason failureReason);
};

gog_AvatarCriteria gog_IFriend_GetDefaultAvatarCriteria(gog_Interface friend);
void gog_IFriend_SetDefaultAvatarCriteria(gog_Interface friend, gog_AvatarCriteria defaultAvatarCriteria);
void gog_IFriend_RequestUserInformation(gog_Interface friend, gog_ID userID, gog_AvatarCriteria avatarCriteria, gog_Interface listener);
bool gog_IFriend_IsUserInformationAvailable(gog_Interface friend, gog_ID userID);
char* gog_IFriend_GetPersonaName(gog_Interface friend);
void gog_IFriend_GetPersonaNameCopy(gog_Interface friend, char* buffer, uint32_t bufferLength);
enum gog_PersonaState gog_IFriend_GetPersonaState(gog_Interface friend);
char* gog_IFriend_GetFriendPersonaName(gog_Interface friend, gog_ID userID);
void gog_IFriend_GetFriendPersonaNameCopy(gog_Interface friend, gog_ID userID, char* buffer, uint32_t bufferLength);
enum gog_PersonaState gog_IFriend_GetFriendPersonaState(gog_Interface friend, gog_ID userID);
char* gog_IFriend_GetFriendAvatarUrl(gog_Interface friend, gog_ID userID, enum gog_AvatarType avatarType);
void gog_IFriend_GetFriendAvatarUrlCopy(gog_Interface friend, gog_ID userID, enum gog_AvatarType avatarType, char* buffer, uint32_t bufferLength);
uint32_t gog_IFriend_GetFriendAvatarImageID(gog_Interface friend, gog_ID userID, enum gog_AvatarType avatarType);
void gog_IFriend_GetFriendAvatarImageRGBA(gog_Interface friend, gog_ID userID, enum gog_AvatarType avatarType, void* buffer, uint32_t bufferLength);
bool gog_IFriend_IsFriendAvatarImageRGBAAvailable(gog_Interface friend, gog_ID userID, enum gog_AvatarType avatarType);
void gog_IFriend_RequestFriendList(gog_Interface friend, gog_Interface listener);
bool gog_IFriend_IsFriend(gog_Interface friend, gog_ID userID);
uint32_t gog_IFriend_GetFriendCount(gog_Interface friend);
gog_ID gog_IFriend_GetFriendByIndex(gog_Interface friend, uint32_t index);
void gog_IFriend_SendFriendInvitation(gog_Interface friend, gog_ID userID, gog_Interface listener);
void gog_IFriend_RequestFriendInvitationList(gog_Interface friend, gog_Interface listener);
void gog_IFriend_RequestSentFriendInvitationList(gog_Interface friend, gog_Interface listener);
uint32_t gog_IFriend_GetFriendInvitationCount(gog_Interface friend);
void gog_IFriend_GetFriendInvitationByIndex(gog_Interface friend, uint32_t index, gog_ID* userID, uint32_t* sendTime);
void gog_IFriend_RespondToFriendInvitation(gog_Interface friend, gog_ID userID, bool accept, gog_Interface listener);
void gog_IFriend_DeleteFriend(gog_Interface friend, gog_ID userID, gog_Interface listener);
void gog_IFriend_SetRichPresence(gog_Interface friend, char* key, char* value, gog_Interface listener);
void gog_IFriend_DeleteRichPresence(gog_Interface friend, char* key, gog_Interface listener);
void gog_IFriend_ClearRichPresence(gog_Interface friend, gog_Interface listener);
void gog_IFriend_RequestRichPresence(gog_Interface friend, gog_ID userID, gog_Interface listener);
char* gog_IFriend_GetRichPresence(gog_Interface friend, char* key, gog_ID userID);
void gog_IFriend_GetRichPresenceCopy(gog_Interface friend, char* key, char* buffer, uint32_t bufferLength, gog_ID userID);
uint32_t gog_IFriend_GetRichPresenceCount(gog_Interface friend, gog_ID userID);
void gog_IFriend_GetRichPresenceByIndex(gog_Interface friend, uint32_t index, char* key, uint32_t keyLength, char* value, uint32_t valueLength, gog_ID userID);
char* gog_IFriend_GetRichPresenceKeyByIndex(gog_Interface friend, uint32_t index, gog_ID userID);
void gog_IFriend_GetRichPresenceKeyByIndexCopy(gog_Interface friend, uint32_t index, char* buffer, uint32_t bufferLength, gog_ID userID);
void gog_IFriend_ShowOverlayInviteDialog(gog_Interface friend, char* connectionString);
void gog_IFriend_SendInvitation(gog_Interface friend, gog_ID userID, char* connectionString, gog_Interface listener);
void gog_IFriend_FindUser(gog_Interface friend, char* userSpecifier, gog_Interface listener);
bool gog_IFriend_IsUserInTheSameGame(gog_Interface friend, gog_ID userID);

//// IChat.h

//// IMatchmaking.h

//// INetworking.h

//// IStats.h

//// IUtils.h

//// IApps.h

//// IStorage.h

//// ICustomNetworking.h

//// ILogger.h

//// ITelemetry.h

//// ICloudStorage.h
