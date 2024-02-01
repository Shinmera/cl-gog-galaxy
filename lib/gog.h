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

enum gog_BasicFailureReason {
  GOG_BASIC_FAILURE_REASON_UNDEFINED, 
  GOG_BASIC_FAILURE_REASON_CONNECTION_FAILURE 
};

// IEncryptedAppTicketListener
// IPlayFabCreateOpenIDConnectionListener
// IPlayFabLoginWithOpenIDConnectListener
struct gog_BasicListener {
  void (*OnEncryptedAppTicketRetrieveSuccess)();
  void (*OnEncryptedAppTicketRetrieveFailure)(enum gog_BasicFailureReason reason);
};

bool gog_IUser_SignedIn();
gog_ID gog_IUser_GetGalaxyID();
void gog_IUser_SignInCredentials(char* login, char* password, gog_Interface listener);
void gog_IUser_SignInToken(char* refreshToken, gog_Interface listener);
void gog_IUser_SignInLauncher(gog_Interface listener);
void gog_IUser_SignInSteam(void* steamAppTicket, uint32_t steamAppTicketSize, char* personaName, gog_Interface listener);
void gog_IUser_SignInGalaxy(bool requireOnline, uint32_t timeout, gog_Interface listener);
void gog_IUser_SignInPS4(char* ps4ClientID, gog_Interface listener);
void gog_IUser_SignInXB1(char* xboxOneUserID, gog_Interface listener);
void gog_IUser_SignInXbox(uint64_t xboxID, gog_Interface listener);
void gog_IUser_SignInXBLive(char* token, char* signature, char* marketplaceID, char* locale, gog_Interface listener);
void gog_IUser_SignInAnonymous(gog_Interface listener);
void gog_IUser_SignInAnonymousTelemetry(gog_Interface listener);
void gog_IUser_SignInServerKey(char* serverKey, gog_Interface listener);
void gog_IUser_SignInAuthorizationCode(char* authorizationCode, char* redirectURI, gog_Interface listener);
void gog_IUser_SignOut();
void gog_IUser_RequestUserData(gog_ID userID, gog_Interface listener);
bool gog_IUser_IsUserDataAvailable(gog_ID userID);
char* gog_IUser_GetUserData(char* key, gog_ID userID);
void gog_IUser_GetUserDataCopy(char* key, char* buffer, uint32_t bufferLength, gog_ID userID);
void gog_IUser_SetUserData(char* key, char* value, gog_Interface listener);
uint32_t gog_IUser_GetUserDataCount(gog_ID userID);
bool gog_IUser_GetUserDataByIndex(uint32_t index, char* key, uint32_t keyLength, char* value, uint32_t valueLength, gog_ID userID);
void gog_IUser_DeleteUserData(char* key, gog_Interface listener);
bool gog_IUser_IsLoggedOn();
void gog_IUser_RequestEncryptedAppTicket(void* data, uint32_t dataSize, gog_Interface listener);
void gog_IUser_GetEncryptedAppTicket(void* encryptedAppTicket, uint32_t maxEncryptedAppTicketSize, uint32_t* currentEncryptedAppTicketSize);
void gog_IUser_CreateOpenIDConnection(char* secretKey, char* titleID, char* connectionID, bool ignoreNonce, gog_Interface listener);
void gog_IUser_LoginWithOpenIDConnect(char* titleID, char* connectionID, char* idToken, bool createAccount, char* encryptedRequest, char* playerSecret, gog_Interface listener);
gog_SessionID gog_IUser_GetSessionID();
char* gog_IUser_GetAccessToken();
void gog_IUser_GetAccessTokenCopy(char* buffer, uint32_t bufferLength);
char* gog_IUser_GetRefreshToken();
void gog_IUser_GetRefreshTokenCopy(char* buffer, uint32_t bufferLength);
char* gog_IUser_GetIDToken();
void gog_IUser_GetIDTokenCopy(char* buffer, uint32_t bufferLength);
bool gog_IUser_ReportInvalidAccessToken(char* accessToken, char* info);

//// IFriends.h

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
