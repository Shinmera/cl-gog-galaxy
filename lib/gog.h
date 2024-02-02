#ifndef __GOG_C_H__
#define __GOG_C_H__
#ifdef __cplusplus
extern "C" {
#endif
#if defined GOG_STATIC
#  define GOG_EXPORT
#elif defined _MSC_VER
#  if defined GOG_BUILD
#    define GOG_EXPORT __declspec(dllexport)
#  else
#    define GOG_EXPORT __declspec(dllimport)
#  endif
#elif defined __GNUC__
#  if defined GOG_BUILD
#    define GOG_EXPORT __attribute__((visibility("default")))
#  else
#    define GOG_EXPORT
#  endif
#else
#  define GOG_EXPORT
#endif
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

static inline gog_ID gog_ID_FromRealID(enum gog_ID_Type type, uint64_t value){
  return (uint64_t)type << 56 | value;
}

static inline uint64_t gog_ID_GetRealID(gog_ID id){
  return id & 0xFFFFFFFFFFFFFF;
}

static inline enum gog_ID_Type gog_ID_GetType(gog_ID id){
  return (enum gog_ID_Type)(id >> 56);
}

static inline bool gog_ID_IsValid(gog_ID id){
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

const char* gog_IError_GetName(gog_Interface error);
const char* gog_IError_GetMsg(gog_Interface error);
enum gog_ErrorType gog_IError_GetType(gog_Interface error);
gog_Interface gog_GetError();

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

enum gog_OperationalState {
  OPERATIONAL_STATE_SIGNED_IN = 0x0001, 
  OPERATIONAL_STATE_LOGGED_ON = 0x0002 
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

enum gog_FriendInvitationFailureReason {
  GOG_FRIENDINVITATION_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_FRIENDINVITATION_FAILURE_REASON_USER_DOES_NOT_EXIST, ///< User does not exist.
  GOG_FRIENDINVITATION_FAILURE_REASON_USER_ALREADY_INVITED, ///< Friend invitation already sent to the user.
  GOG_FRIENDINVITATION_FAILURE_REASON_USER_ALREADY_FRIEND, ///< User already on the friend list.
  GOG_FRIENDINVITATION_FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

enum gog_FriendInvitationRespondFailureReason {
  GOG_FRIENDINVITATIONRESPOND_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_FRIENDINVITATIONRESPOND_FAILURE_REASON_USER_DOES_NOT_EXIST, ///< User does not exist.
  GOG_FRIENDINVITATIONRESPOND_FAILURE_REASON_FRIEND_INVITATION_DOES_NOT_EXIST, ///< Friend invitation does not exist.
  GOG_FRIENDINVITATIONRESPOND_FAILURE_REASON_USER_ALREADY_FRIEND, ///< User already on the friend list.
  GOG_FRIENDINVITATIONRESPOND_FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

enum gog_InvitationDirection{
  GOG_INVITATION_DIRECTION_INCOMING, ///< The user indicated in the notification was the inviter.
  GOG_INVITATION_DIRECTION_OUTGOING ///< The user indicated in the notification was the invitee.
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

enum gog_UserFindFailureReason {
  GOG_USERFIND_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_USERFIND_FAILURE_REASON_USER_NOT_FOUND, ///< Specified user was not found.
  GOG_USERFIND_FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

gog_AvatarCriteria gog_IFriends_GetDefaultAvatarCriteria(gog_Interface _friend);
void gog_IFriends_SetDefaultAvatarCriteria(gog_Interface _friend, gog_AvatarCriteria defaultAvatarCriteria);
void gog_IFriends_RequestUserInformation(gog_Interface _friend, gog_ID userID, gog_AvatarCriteria avatarCriteria, gog_Interface listener);
bool gog_IFriends_IsUserInformationAvailable(gog_Interface _friend, gog_ID userID);
char* gog_IFriends_GetPersonaName(gog_Interface _friend);
void gog_IFriends_GetPersonaNameCopy(gog_Interface _friend, char* buffer, uint32_t bufferLength);
enum gog_PersonaState gog_IFriends_GetPersonaState(gog_Interface _friend);
char* gog_IFriends_GetFriendPersonaName(gog_Interface _friend, gog_ID userID);
void gog_IFriends_GetFriendPersonaNameCopy(gog_Interface _friend, gog_ID userID, char* buffer, uint32_t bufferLength);
enum gog_PersonaState gog_IFriends_GetFriendPersonaState(gog_Interface _friend, gog_ID userID);
char* gog_IFriends_GetFriendAvatarUrl(gog_Interface _friend, gog_ID userID, enum gog_AvatarType avatarType);
void gog_IFriends_GetFriendAvatarUrlCopy(gog_Interface _friend, gog_ID userID, enum gog_AvatarType avatarType, char* buffer, uint32_t bufferLength);
uint32_t gog_IFriends_GetFriendAvatarImageID(gog_Interface _friend, gog_ID userID, enum gog_AvatarType avatarType);
void gog_IFriends_GetFriendAvatarImageRGBA(gog_Interface _friend, gog_ID userID, enum gog_AvatarType avatarType, void* buffer, uint32_t bufferLength);
bool gog_IFriends_IsFriendAvatarImageRGBAAvailable(gog_Interface _friend, gog_ID userID, enum gog_AvatarType avatarType);
void gog_IFriends_RequestFriendList(gog_Interface _friend, gog_Interface listener);
bool gog_IFriends_IsFriend(gog_Interface _friend, gog_ID userID);
uint32_t gog_IFriends_GetFriendCount(gog_Interface _friend);
gog_ID gog_IFriends_GetFriendByIndex(gog_Interface _friend, uint32_t index);
void gog_IFriends_SendFriendInvitation(gog_Interface _friend, gog_ID userID, gog_Interface listener);
void gog_IFriends_RequestFriendInvitationList(gog_Interface _friend, gog_Interface listener);
void gog_IFriends_RequestSentFriendInvitationList(gog_Interface _friend, gog_Interface listener);
uint32_t gog_IFriends_GetFriendInvitationCount(gog_Interface _friend);
void gog_IFriends_GetFriendInvitationByIndex(gog_Interface _friend, uint32_t index, gog_ID* userID, uint32_t* sendTime);
void gog_IFriends_RespondToFriendInvitation(gog_Interface _friend, gog_ID userID, bool accept, gog_Interface listener);
void gog_IFriends_DeleteFriend(gog_Interface _friend, gog_ID userID, gog_Interface listener);
void gog_IFriends_SetRichPresence(gog_Interface _friend, char* key, char* value, gog_Interface listener);
void gog_IFriends_DeleteRichPresence(gog_Interface _friend, char* key, gog_Interface listener);
void gog_IFriends_ClearRichPresence(gog_Interface _friend, gog_Interface listener);
void gog_IFriends_RequestRichPresence(gog_Interface _friend, gog_ID userID, gog_Interface listener);
char* gog_IFriends_GetRichPresence(gog_Interface _friend, char* key, gog_ID userID);
void gog_IFriends_GetRichPresenceCopy(gog_Interface _friend, char* key, char* buffer, uint32_t bufferLength, gog_ID userID);
uint32_t gog_IFriends_GetRichPresenceCount(gog_Interface _friend, gog_ID userID);
void gog_IFriends_GetRichPresenceByIndex(gog_Interface _friend, uint32_t index, char* key, uint32_t keyLength, char* value, uint32_t valueLength, gog_ID userID);
char* gog_IFriends_GetRichPresenceKeyByIndex(gog_Interface _friend, uint32_t index, gog_ID userID);
void gog_IFriends_GetRichPresenceKeyByIndexCopy(gog_Interface _friend, uint32_t index, char* buffer, uint32_t bufferLength, gog_ID userID);
void gog_IFriends_ShowOverlayInviteDialog(gog_Interface _friend, char* connectionString);
void gog_IFriends_SendInvitation(gog_Interface _friend, gog_ID userID, char* connectionString, gog_Interface listener);
void gog_IFriends_FindUser(gog_Interface _friend, char* userSpecifier, gog_Interface listener);
bool gog_IFriends_IsUserInTheSameGame(gog_Interface _friend, gog_ID userID);

//// IChat.h
typedef uint64_t gog_ChatRoomID;
typedef uint64_t gog_ChatMessageID;

enum gog_ChatMessageType {
  GOG_CHAT_MESSAGE_TYPE_UNKNOWN, ///< Unknown message type.
  GOG_CHAT_MESSAGE_TYPE_CHAT_MESSAGE, ///< Chat message.
  GOG_CHAT_MESSAGE_TYPE_GAME_INVITATION ///< Game invitation.
};

enum gog_ChatUserRetrieveFailureReason {
  GOG_CHATUSERRETRIEVE_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_CHATUSERRETRIEVE_FAILURE_REASON_FORBIDDEN, ///< Communication with a specified user is not allowed.
  GOG_CHATUSERRETRIEVE_FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

enum gog_ChatMessageSendFailureReason {
  GOG_CHATMESSAGESEND_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_CHATMESSAGESEND_FAILURE_REASON_FORBIDDEN, ///< Sending messages to the chat room is forbidden for the user.
  GOG_CHATMESSAGESEND_FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

enum gog_ChatMessageRetrieveFailureReason {
  FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  FAILURE_REASON_FORBIDDEN, ///< Retrieving messages from the chat room is forbidden for the user.
  FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

void gog_IChat_RequestChatRoomWithUser(gog_Interface chat, gog_ID userID, gog_Interface listener);
void gog_IChat_RequestChatRoomMessages(gog_Interface chat, gog_ChatRoomID chatRoomID, uint32_t limit, gog_ChatMessageID referenceMessageID, gog_Interface listener);
uint32_t gog_IChat_SendChatRoomMessage(gog_Interface chat, gog_ChatRoomID chatRoomID, const char* msg, gog_Interface listener);
uint32_t gog_IChat_GetChatRoomMessageByIndex(gog_Interface chat, uint32_t index, gog_ChatMessageID* messageID, enum gog_ChatMessageType* messageType, gog_ID* senderID, uint32_t* sendTime, char* buffer, uint32_t bufferLength);
uint32_t gog_IChat_GetChatRoomMemberCount(gog_Interface chat, gog_ChatRoomID chatRoomID);
gog_ID gog_IChat_GetChatRoomMemberUserIDByIndex(gog_Interface chat, gog_ChatRoomID chatRoomID, uint32_t index);
uint32_t gog_IChat_GetChatRoomUnreadMessageCount(gog_Interface chat, gog_ChatRoomID chatRoomID);
void gog_IChat_MarkChatRoomAsRead(gog_Interface chat, gog_ChatRoomID chatRoomID);

//// IMatchmaking.h
enum gog_LobbyType {
  GOG_LOBBY_TYPE_PRIVATE, ///< Only invited users are able to join the lobby.
  GOG_LOBBY_TYPE_FRIENDS_ONLY, ///< Visible only to friends or invitees, but not in lobby list. Forbidden for the Game Server.
  GOG_LOBBY_TYPE_PUBLIC, ///< Visible for friends and in lobby list.
  GOG_LOBBY_TYPE_INVISIBLE_TO_FRIENDS ///< Returned by search, but not visible to friends. Forbidden for the Game Server.
};

enum gog_LobbyTopologyType {
  GOG_DEPRECATED_LOBBY_TOPOLOGY_TYPE_FCM_HOST_MIGRATION, ///< All users are connected with each other. Disconnection of lobby owner results in choosing a new one. Deprecated: use LOBBY_TOPOLOGY_TYPE_FCM_OWNERSHIP_TRANSITION instead.
  GOG_LOBBY_TOPOLOGY_TYPE_FCM, ///< All users are connected with each other. Disconnection of lobby owner results in closing the lobby.
  GOG_LOBBY_TOPOLOGY_TYPE_STAR, ///< All users are connected with lobby owner. Disconnection of lobby owner results in closing the lobby.
  GOG_LOBBY_TOPOLOGY_TYPE_CONNECTIONLESS, ///< All users are connected only with server. Disconnection of lobby owner results in choosing a new one. Forbidden for the Game Server.
  GOG_LOBBY_TOPOLOGY_TYPE_FCM_OWNERSHIP_TRANSITION ///< All users are connected with each other. Disconnection of lobby owner results in choosing a new one. Forbidden for the Game Server.
};

enum gog_LobbyMemberStateChange {
  GOG_LOBBY_MEMBER_STATE_CHANGED_ENTERED = 0x0001, ///< The user joined the lobby.
  GOG_LOBBY_MEMBER_STATE_CHANGED_LEFT = 0x0002, ///< The user left the lobby having announced it first.
  GOG_LOBBY_MEMBER_STATE_CHANGED_DISCONNECTED = 0x0004, ///< The user disconnected without leaving the lobby first.
  GOG_LOBBY_MEMBER_STATE_CHANGED_KICKED = 0x0008, ///< User was kicked from the lobby.
  GOG_LOBBY_MEMBER_STATE_CHANGED_BANNED = 0x0010 ///< User was kicked and banned from the lobby.
};

enum gog_LobbyComparisonType {
  GOG_LOBBY_COMPARISON_TYPE_EQUAL, ///< The lobby should have a property of a value that is equal to the one specified.
  GOG_LOBBY_COMPARISON_TYPE_NOT_EQUAL, ///< The lobby should have a property of a value that is not equal to the one specified.
  GOG_LOBBY_COMPARISON_TYPE_GREATER, ///< The lobby should have a property of a value that is greater than the one specified.
  GOG_LOBBY_COMPARISON_TYPE_GREATER_OR_EQUAL, ///< The lobby should have a property of a value that is greater than or equal to the one specified.
  GOG_LOBBY_COMPARISON_TYPE_LOWER, ///< The lobby should have a property of a value that is lower than the one specified.
  GOG_LOBBY_COMPARISON_TYPE_LOWER_OR_EQUAL ///< The lobby should have a property of a value that is lower than or equal to the one specified.
};

enum gog_LobbyCreateResult {
  GOG_LOBBY_CREATE_RESULT_SUCCESS, ///< Lobby was created.
  GOG_LOBBY_CREATE_RESULT_ERROR, ///< Unexpected error.
  GOG_LOBBY_CREATE_RESULT_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

enum gog_LobbyEnterResult {
  GOG_LOBBY_ENTER_RESULT_SUCCESS, ///< The user has entered the lobby.
  GOG_LOBBY_ENTER_RESULT_LOBBY_DOES_NOT_EXIST, ///< Specified lobby does not exist.
  GOG_LOBBY_ENTER_RESULT_LOBBY_IS_FULL, ///< Specified lobby is full.
  GOG_LOBBY_ENTER_RESULT_ERROR, ///< Unexpected error.
  GOG_LOBBY_ENTER_RESULT_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

enum gog_LobbyListResult {
  GOG_LOBBY_LIST_RESULT_SUCCESS, ///< The list of lobbies retrieved successfully.
  GOG_LOBBY_LIST_RESULT_ERROR, ///< Unexpected error.
  GOG_LOBBY_LIST_RESULT_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

enum gog_LobbyLeaveReason {
  GOG_LOBBY_LEAVE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_LOBBY_LEAVE_REASON_USER_LEFT, ///< The user has left the lobby as a result of calling IMatchmaking::LeaveLobby().
  GOG_LOBBY_LEAVE_REASON_LOBBY_CLOSED, ///< The lobby has been closed (e.g. the owner has left the lobby without host migration).
  GOG_LOBBY_LEAVE_REASON_CONNECTION_LOST ///< The Peer has lost the connection.
};

enum gog_LobbyDataUpdateFailureReason {
  GOG_LOBBYDATAUPDATE_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_LOBBYDATAUPDATE_FAILURE_REASON_LOBBY_DOES_NOT_EXIST, ///< Specified lobby does not exist.
  GOG_LOBBYDATAUPDATE_FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

enum gog_LobbyDataRetrieveFailureReason {
  GOG_LOBBYDATARETRIEVE_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_LOBBYDATARETRIEVE_FAILURE_REASON_LOBBY_DOES_NOT_EXIST, ///< Specified lobby does not exist.
  GOG_LOBBYDATARETRIEVE_FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

void gog_IMatchmaking_CreateLobby(gog_Interface matchmaking, enum gog_LobbyType lobbyType, uint32_t maxMembers, bool joinable, enum gog_LobbyTopologyType lobbyTopologyType, gog_Interface created, gog_Interface entered);
void gog_IMatchmaking_RequestLobbyList(gog_Interface matchmaking, bool allowFullLobbies, gog_Interface listener);
void gog_IMatchmaking_AddRequestLobbyListResultCountFilter(gog_Interface matchmaking, uint32_t limit);
void gog_IMatchmaking_AddRequestLobbyListStringFilter(gog_Interface matchmaking, char* keyToMatch, char* valueToMatch, enum gog_LobbyComparisonType comparisonType);
void gog_IMatchmaking_AddRequestLobbyListNumericalFilter(gog_Interface matchmaking, char* keyToMatch, int32_t valueToMatch, enum gog_LobbyComparisonType comparisonType);
void gog_IMatchmaking_AddRequestLobbyListNearValueFilter(gog_Interface matchmaking, char* keyToMatch, int32_t valueToBeCloseTo);
gog_ID gog_IMatchmaking_GetLobbyByIndex(gog_Interface matchmaking, uint32_t index);
void gog_IMatchmaking_JoinLobby(gog_Interface matchmaking, gog_ID lobbyID, gog_Interface listener);
void gog_IMatchmaking_LeaveLobby(gog_Interface matchmaking, gog_ID lobbyID, gog_Interface listener);
void gog_IMatchmaking_SetMaxNumLobbyMembers(gog_Interface matchmaking, gog_ID lobbyID, uint32_t maxNumLobbyMembers, gog_Interface listener);
uint32_t gog_IMatchmaking_GetMaxNumLobbyMembers(gog_Interface matchmaking, gog_ID lobbyID);
uint32_t gog_IMatchmaking_GetNumLobbyMembers(gog_Interface matchmaking, gog_ID lobbyID);
gog_ID gog_IMatchmaking_GetLobbyMemberByIndex(gog_Interface matchmaking, gog_ID lobbyID, uint32_t index);
void gog_IMatchmaking_SetLobbyType(gog_Interface matchmaking, gog_ID lobbyID, enum gog_LobbyType lobbyType, gog_Interface listener);
enum gog_LobbyType gog_IMatchmaking_GetLobbyType(gog_Interface matchmaking, gog_ID lobbyID);
void gog_IMatchmaking_SetLobbyJoinable(gog_Interface matchmaking, gog_ID lobbyID, bool joinable, gog_Interface listener);
bool gog_IMatchmaking_IsLobbyJoinable(gog_Interface matchmaking, gog_ID lobbyID);
void gog_IMatchmaking_RequestLobbyData(gog_Interface matchmaking, gog_ID lobbyID, gog_Interface listener);
char* gog_IMatchmaking_GetLobbyData(gog_Interface matchmaking, gog_ID lobbyID, char* key);
void gog_IMatchmaking_GetLobbyDataCopy(gog_Interface matchmaking, gog_ID lobbyID, char* key, char* buffer, uint32_t bufferLength);
void gog_IMatchmaking_SetLobbyData(gog_Interface matchmaking, gog_ID lobbyID, char* key, char* value, gog_Interface listener);
uint32_t gog_IMatchmaking_GetLobbyDataCount(gog_Interface matchmaking, gog_ID lobbyID);
bool gog_IMatchmaking_GetLobbyDataByIndex(gog_Interface matchmaking, gog_ID lobbyID, uint32_t index, char* key, uint32_t keyLength, char* value, uint32_t valueLength);
void gog_IMatchmaking_DeleteLobbyData(gog_Interface matchmaking, gog_ID lobbyID, char* key, gog_Interface listener);
char* gog_IMatchmaking_GetLobbyMemberData(gog_Interface matchmaking, gog_ID lobbyID, gog_ID memberID, char* key);
void gog_IMatchmaking_GetLobbyMemberDataCopy(gog_Interface matchmaking, gog_ID lobbyID, gog_ID memberID, char* key, char* buffer, uint32_t bufferLength);
void gog_IMatchmaking_SetLobbyMemberData(gog_Interface matchmaking, gog_ID lobbyID, char* key, char* value, gog_Interface listener);
uint32_t gog_IMatchmaking_GetLobbyMemberDataCount(gog_Interface matchmaking, gog_ID lobbyID, gog_ID memberID);
bool gog_IMatchmaking_GetLobbyMemberDataByIndex(gog_Interface matchmaking, gog_ID lobbyID, gog_ID memberID, uint32_t index, char* key, uint32_t keyLength, char* value, uint32_t valueLength);
void gog_IMatchmaking_DeleteLobbyMemberData(gog_Interface matchmaking, gog_ID lobbyID, char* key, gog_Interface listener);
gog_ID gog_IMatchmaking_GetLobbyOwner(gog_Interface matchmaking, gog_ID lobbyID);
bool gog_IMatchmaking_SendLobbyMessage(gog_Interface matchmaking, gog_ID lobbyID, void* data, uint32_t dataSize);
uint32_t gog_IMatchmaking_GetLobbyMessage(gog_Interface matchmaking, gog_ID lobbyID, uint32_t messageID, gog_ID senderID, char* msg, uint32_t msgLength);

//// INetworking.h
enum gog_NatType {
  GOG_NAT_TYPE_NONE, ///< There is no NAT at all.
  GOG_NAT_TYPE_FULL_CONE, ///< Accepts any datagrams to a port that has been previously used.
  GOG_NAT_TYPE_ADDRESS_RESTRICTED, ///< Accepts datagrams to a port if the datagram source IP address belongs to a system that has been sent to.
  GOG_NAT_TYPE_PORT_RESTRICTED, ///< Accepts datagrams to a port if the datagram source IP address and port belongs a system that has been sent to.
  GOG_NAT_TYPE_SYMMETRIC, ///< A different port is chosen for every remote destination.
  GOG_NAT_TYPE_UNKNOWN ///< NAT type has not been determined.
};

enum gog_P2PSendType {
  GOG_P2P_SEND_UNRELIABLE, ///< UDP-like packet transfer. The packet will be sent during the next call to ProcessData().
  GOG_P2P_SEND_RELIABLE, ///< TCP-like packet transfer. The packet will be sent during the next call to ProcessData().
  GOG_P2P_SEND_UNRELIABLE_IMMEDIATE, ///< UDP-like packet transfer. The packet will be sent instantly.
  GOG_P2P_SEND_RELIABLE_IMMEDIATE ///< TCP-like packet transfer. The packet will be sent instantly.
};

enum gog_ConnectionType {
  GOG_CONNECTION_TYPE_NONE, ///< User is connected only with server.
  GOG_CONNECTION_TYPE_DIRECT, ///< User is connected directly.
  GOG_CONNECTION_TYPE_PROXY ///< User is connected through a proxy.
};

bool gog_INetworking_SendP2PPacket(gog_Interface networking, gog_ID galaxyID, const void* data, uint32_t dataSize, enum gog_P2PSendType sendType, uint8_t channel);
bool gog_INetworking_PeekP2PPacket(gog_Interface networking, void* dest, uint32_t destSize, uint32_t* outMsgSize, gog_ID outGalaxyID, uint8_t channel);
bool gog_INetworking_IsP2PPacketAvailable(gog_Interface networking, uint32_t* outMsgSize, uint8_t channel);
bool gog_INetworking_ReadP2PPacket(gog_Interface networking, void* dest, uint32_t destSize, uint32_t* outMsgSize, gog_ID outGalaxyID, uint8_t channel);
void gog_INetworking_PopP2PPacket(gog_Interface networking, uint8_t channel);
int gog_INetworking_GetPingWith(gog_Interface networking, gog_ID galaxyID);
void gog_INetworking_RequestNatTypeDetection(gog_Interface networking);
enum gog_NatType gog_INetworking_GetNatType(gog_Interface networking);
enum gog_ConnectionType gog_INetworking_GetConnectionType(gog_Interface networking, gog_ID userID);

//// IStats.h
enum gog_LeaderboardSortMethod {
  GOG_LEADERBOARD_SORT_METHOD_NONE, ///< No sorting method specified.
  GOG_LEADERBOARD_SORT_METHOD_ASCENDING, ///< Top score is lowest number.
  GOG_LEADERBOARD_SORT_METHOD_DESCENDING ///< Top score is highest number.
};

enum gog_LeaderboardDisplayType {
  GOG_LEADERBOARD_DISPLAY_TYPE_NONE, ///< Not display type specified.
  GOG_LEADERBOARD_DISPLAY_TYPE_NUMBER, ///< Simple numerical score.
  GOG_LEADERBOARD_DISPLAY_TYPE_TIME_SECONDS, ///< The score represents time, in seconds.
  GOG_LEADERBOARD_DISPLAY_TYPE_TIME_MILLISECONDS ///< The score represents time, in milliseconds.
};

enum gog_LeaderboardScoreUpdateFailureReason {
  GOG_LEADERBOARDSCOREUPDATE_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_LEADERBOARDSCOREUPDATE_FAILURE_REASON_NO_IMPROVEMENT, ///< Previous score was better and the update operation was not forced.
  GOG_LEADERBOARDSCOREUPDATE_FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

void gog_IStats_RequestUserStatsAndAchievements(gog_Interface stats, gog_ID userID, gog_Interface listener);
int32_t gog_IStats_GetStatInt(gog_Interface stats, char* name, gog_ID userID);
float gog_IStats_GetStatFloat(gog_Interface stats, char* name, gog_ID userID);
void gog_IStats_SetStatInt(gog_Interface stats, char* name, int32_t value);
void gog_IStats_SetStatFloat(gog_Interface stats, char* name, float value);
void gog_IStats_UpdateAvgRateStat(gog_Interface stats, char* name, float countThisSession, double sessionLength);
void gog_IStats_GetAchievement(gog_Interface stats, char* name, bool* unlocked, uint32_t* unlockTime, gog_ID userID);
void gog_IStats_SetAchievement(gog_Interface stats, char* name);
void gog_IStats_ClearAchievement(gog_Interface stats, char* name);
void gog_IStats_StoreStatsAndAchievements(gog_Interface stats, gog_Interface listener);
void gog_IStats_ResetStatsAndAchievements(gog_Interface stats, gog_Interface listener);
char* gog_IStats_GetAchievementDisplayName(gog_Interface stats, char* name);
void gog_IStats_GetAchievementDisplayNameCopy(gog_Interface stats, char* name, char* buffer, uint32_t bufferLength);
char* gog_IStats_GetAchievementDescription(gog_Interface stats, char* name);
void gog_IStats_GetAchievementDescriptionCopy(gog_Interface stats, char* name, char* buffer, uint32_t bufferLength);
bool gog_IStats_IsAchievementVisible(gog_Interface stats, char* name);
bool gog_IStats_IsAchievementVisibleWhileLocked(gog_Interface stats, char* name);
void gog_IStats_RequestLeaderboards(gog_Interface stats, gog_Interface listener);
char* gog_IStats_GetLeaderboardDisplayName(gog_Interface stats, char* name);
void gog_IStats_GetLeaderboardDisplayNameCopy(gog_Interface stats, char* name, char* buffer, uint32_t bufferLength);
enum gog_LeaderboardSortMethod gog_IStats_GetLeaderboardSortMethod(gog_Interface stats, char* name);
enum gog_LeaderboardDisplayType gog_IStats_GetLeaderboardDisplayType(gog_Interface stats, char* name);
void gog_IStats_RequestLeaderboardEntriesGlobal(gog_Interface stats, char* name, uint32_t rangeStart, uint32_t rangeEnd, gog_Interface listener);
void gog_IStats_RequestLeaderboardEntriesAroundUser(gog_Interface stats, char* name, uint32_t countBefore, uint32_t countAfter, gog_ID userID, gog_Interface listener);
void gog_IStats_RequestLeaderboardEntriesForUsers(gog_Interface stats, char* name, gog_ID* userArray, uint32_t userArraySize, gog_Interface listener);
void gog_IStats_GetRequestedLeaderboardEntry(gog_Interface stats, uint32_t index, uint32_t* rank, int32_t* score, gog_ID* userID);
void gog_IStats_GetRequestedLeaderboardEntryWithDetails(gog_Interface stats, uint32_t index, uint32_t* rank, int32_t* score, void* details, uint32_t detailsSize, uint32_t* outDetailsSize, gog_ID* userID);
void gog_IStats_SetLeaderboardScore(gog_Interface stats, char* name, int32_t score, bool forceUpdate, gog_Interface listener);
void gog_IStats_SetLeaderboardScoreWithDetails(gog_Interface stats, char* name, int32_t score, void* details, uint32_t detailsSize, bool forceUpdate, gog_Interface listener);
uint32_t gog_IStats_GetLeaderboardEntryCount(gog_Interface stats, char* name);
void gog_IStats_FindLeaderboard(gog_Interface stats, char* name, gog_Interface listener);
void gog_IStats_FindOrCreateLeaderboard(gog_Interface stats, char* name, char* displayName, enum gog_LeaderboardSortMethod sortMethod, enum gog_LeaderboardDisplayType displayType, gog_Interface listener);
void gog_IStats_RequestUserTimePlayed(gog_Interface stats, gog_ID userID, gog_Interface listener);
uint32_t gog_IStats_GetUserTimePlayed(gog_Interface stats, gog_ID userID);

//// IUtils.h
typedef uint64_t gog_NotificationID;

enum gog_OverlayState {
  GOG_OVERLAY_STATE_UNDEFINED, ///< Overlay state is undefined.
  GOG_OVERLAY_STATE_NOT_SUPPORTED, ///< Overlay is not supported.
  GOG_OVERLAY_STATE_DISABLED, ///< Overlay is disabled by the user in the Galaxy Client.
  GOG_OVERLAY_STATE_FAILED_TO_INITIALIZE, ///< Galaxy Client failed to initialize the overlay or inject it into the game.
  GOG_OVERLAY_STATE_INITIALIZED ///< Overlay has been successfully injected into the game.
};

enum gog_ServicesConnectionState {
  GOG_SERVICES_CONNECTION_STATE_UNDEFINED, ///< Connection state is undefined.
  GOG_SERVICES_CONNECTION_STATE_CONNECTED, ///< Connection to the GOG services has been established.
  GOG_SERVICES_CONNECTION_STATE_DISCONNECTED, ///< Connection to the GOG services has been lost.
  GOG_SERVICES_CONNECTION_STATE_AUTH_LOST ///< Connection to the GOG services has been lost due to lose of peer authentication.
};

void gog_IUtils_GetImageSize(gog_Interface utils, uint32_t imageID, int32_t *width, int32_t *height);
void gog_IUtils_GetImageRGBA(gog_Interface utils, uint32_t imageID, void* buffer, uint32_t bufferLength);
void gog_IUtils_RegisterForNotification(gog_Interface utils, char* type);
uint32_t gog_IUtils_GetNotification(gog_Interface utils, gog_NotificationID notificationID, bool *consumable, char* type, uint32_t typeLength, void* content, uint32_t contentSize);
void gog_IUtils_ShowOverlayWithWebPage(gog_Interface utils, char* url);
bool gog_IUtils_IsOverlayVisible(gog_Interface utils);
enum gog_OverlayState gog_IUtils_GetOverlayState(gog_Interface utils);
void gog_IUtils_DisableOverlayPopups(gog_Interface utils, char* popupGroup);
enum gog_ServicesConnectionState gog_IUtils_GetGogServicesConnectionState(gog_Interface utils);

//// IApps.h
typedef uint64_t gog_ProductID;

enum gog_DlcCheckFailureReason{
  GOG_DLCCHECK_FAILURE_REASON_UNDEFINED, ///< Undefined error.
  GOG_DLCCHECK_FAILURE_REASON_GALAXY_SERVICE_NOT_SIGNED_IN, ///< Galaxy Service is not signed in properly.
  GOG_DLCCHECK_FAILURE_REASON_CONNECTION_FAILURE, ///< Unable to communicate with backend services.
  GOG_DLCCHECK_FAILURE_REASON_EXTERNAL_SERVICE_FAILURE ///< Unable to communicate with external service.
};

bool gog_IApps_IsDlcInstalled(gog_Interface apps, gog_ProductID productID);
void gog_IApps_IsDlcOwned(gog_Interface apps, gog_ProductID productID, gog_Interface listener);
char* gog_IApps_GetCurrentGameLanguage(gog_Interface apps, gog_ProductID productID);
void gog_IApps_GetCurrentGameLanguageCopy(gog_Interface apps, char* buffer, uint32_t bufferLength, gog_ProductID productID);
char* gog_IApps_GetCurrentGameLanguageCode(gog_Interface apps, gog_ProductID productID);
void gog_IApps_GetCurrentGameLanguageCodeCopy(gog_Interface apps, char* buffer, uint32_t bufferLength, gog_ProductID productID);

//// IStorage.h
typedef uint64_t gog_SharedFileID;

void gog_IStorage_FileWrite(gog_Interface storage, const char* fileName, const void* data, uint32_t dataSize);
uint32_t gog_IStorage_FileRead(gog_Interface storage, const char* fileName, void* data, uint32_t dataSize);
void gog_IStorage_FileDelete(gog_Interface storage, const char* fileName);
bool gog_IStorage_FileExists(gog_Interface storage, const char* fileName);
uint32_t gog_IStorage_GetFileSize(gog_Interface storage, const char* fileName);
uint32_t gog_IStorage_GetFileTimestamp(gog_Interface storage, const char* fileName);
uint32_t gog_IStorage_GetFileCount(gog_Interface storage);
char* gog_IStorage_GetFileNameByIndex(gog_Interface storage, uint32_t index);
void gog_IStorage_GetFileNameCopyByIndex(gog_Interface storage, uint32_t index, char* buffer, uint32_t bufferLength);
void gog_IStorage_FileShare(gog_Interface storage, const char* fileName, gog_Interface listener);
void gog_IStorage_DownloadSharedFile(gog_Interface storage, gog_SharedFileID sharedFileID, gog_Interface listener);
char* gog_IStorage_GetSharedFileName(gog_Interface storage, gog_SharedFileID sharedFileID);
void gog_IStorage_GetSharedFileNameCopy(gog_Interface storage, gog_SharedFileID sharedFileID, char* buffer, uint32_t bufferLength);
uint32_t gog_IStorage_GetSharedFileSize(gog_Interface storage, gog_SharedFileID sharedFileID);
gog_ID gog_IStorage_GetSharedFileOwner(gog_Interface storage, gog_SharedFileID sharedFileID);
uint32_t gog_IStorage_SharedFileRead(gog_Interface storage, gog_SharedFileID sharedFileID, void* data, uint32_t dataSize, uint32_t offset);
void gog_IStorage_SharedFileClose(gog_Interface storage, gog_SharedFileID sharedFileID);
uint32_t gog_IStorage_GetDownloadedSharedFileCount(gog_Interface storage);
gog_SharedFileID gog_IStorage_GetDownloadedSharedFileByIndex(gog_Interface storage, uint32_t index);

//// ICustomNetworking.h
typedef uint64_t gog_ConnectionID;

enum gog_ConnectionOpenFailureReason {
  GOG_CONNECTIONOPEN_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_CONNECTIONOPEN_FAILURE_REASON_CONNECTION_FAILURE, ///< Unable to communicate with backend services.
  GOG_CONNECTIONOPEN_FAILURE_REASON_UNAUTHORIZED ///< Client is unauthorized.
};

enum gog_CloseReason {
  CLOSE_REASON_UNDEFINED ///< Unspecified reason.
};

void gog_ICustomNetworking_OpenConnection(gog_Interface customnetworking, const char* connectionString, gog_Interface listener);
void gog_ICustomNetworking_CloseConnection(gog_Interface customnetworking, gog_ConnectionID connectionID, gog_Interface listener);
void gog_ICustomNetworking_SendData(gog_Interface customnetworking, gog_ConnectionID connectionID, const void* data, uint32_t dataSize);
uint32_t gog_ICustomNetworking_GetAvailableDataSize(gog_Interface customnetworking, gog_ConnectionID connectionID);
void gog_ICustomNetworking_PeekData(gog_Interface customnetworking, gog_ConnectionID connectionID, void* dest, uint32_t dataSize);
void gog_ICustomNetworking_ReadData(gog_Interface customnetworking, gog_ConnectionID connectionID, void* dest, uint32_t dataSize);
void gog_ICustomNetworking_PopData(gog_Interface customnetworking, gog_ConnectionID connectionID, uint32_t dataSize);

//// ILogger.h
void gog_ILogger_Trace(gog_Interface logger, const char* format, ...);
void gog_ILogger_Debug(gog_Interface logger, const char* format, ...);
void gog_ILogger_Info(gog_Interface logger, const char* format, ...);
void gog_ILogger_Warning(gog_Interface logger, const char* format, ...);
void gog_ILogger_Error(gog_Interface logger, const char* format, ...);
void gog_ILogger_Fatal(gog_Interface logger, const char* format, ...);

//// ITelemetry.h
enum gog_TelemetryEventFailureReason {
  GOG_TELEMETRYEVENT_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_TELEMETRYEVENT_FAILURE_REASON_CLIENT_FORBIDDEN, ///< Sending telemetry events is forbidden for this application.
  GOG_TELEMETRYEVENT_FAILURE_REASON_INVALID_DATA, ///< The event of given type and form does not match specification.
  GOG_TELEMETRYEVENT_FAILURE_REASON_CONNECTION_FAILURE, ///< Unable to communicate with backend services.
  GOG_TELEMETRYEVENT_FAILURE_REASON_NO_SAMPLING_CLASS_IN_CONFIG, ///< The event sampling class not present in configuration.
  GOG_TELEMETRYEVENT_FAILURE_REASON_SAMPLING_CLASS_FIELD_MISSING, ///< Sampling class' field not present in the event.
  GOG_TELEMETRYEVENT_FAILURE_REASON_EVENT_SAMPLED_OUT, ///< The event did not match sampling criteria.
  GOG_TELEMETRYEVENT_FAILURE_REASON_SAMPLING_RESULT_ALREADY_EXIST, ///< The event already contains a field defined as "dry_run_result_path".
  GOG_TELEMETRYEVENT_FAILURE_REASON_SAMPLING_INVALID_RESULT_PATH ///< "dry_run_result_path" has an invalid value (doesn't start with "data" or "meta" or one of the intermediate elements isn't an object).
};

void gog_ITelemetry_AddStringParam(gog_Interface telemetry, const char* name, const char* value);
void gog_ITelemetry_AddIntParam(gog_Interface telemetry, const char* name, int32_t value);
void gog_ITelemetry_AddFloatParam(gog_Interface telemetry, const char* name, double value);
void gog_ITelemetry_AddBoolParam(gog_Interface telemetry, const char* name, bool value);
void gog_ITelemetry_AddObjectParam(gog_Interface telemetry, const char* name);
void gog_ITelemetry_AddArrayParam(gog_Interface telemetry, const char* name);
void gog_ITelemetry_CloseParam(gog_Interface telemetry);
void gog_ITelemetry_ClearParams(gog_Interface telemetry);
void gog_ITelemetry_SetSamplingClass(gog_Interface telemetry, const char* name);
uint32_t gog_ITelemetry_SendTelemetryEvent(gog_Interface telemetry, const char* eventType, gog_Interface listener);
uint32_t gog_ITelemetry_SendAnonymousTelemetryEvent(gog_Interface telemetry, const char* eventType, gog_Interface listener);
char* gog_ITelemetry_GetVisitID(gog_Interface telemetry);
void gog_ITelemetry_GetVisitIDCopy(gog_Interface telemetry, char* buffer, uint32_t bufferLength);
void gog_ITelemetry_ResetVisitID(gog_Interface telemetry);

//// ICloudStorage.h
enum gog_SavegameType {
  GOG_SAVEGAME_TYPE_UNDEFINED, ///< Not defined savegame type.
  GOG_SAVEGAME_TYPE_AUTO, ///< Auto savegame, triggered automatically over time.
  GOG_SAVEGAME_TYPE_QUICK, ///< Quick savegame.
  GOG_SAVEGAME_TYPE_MANUAL, ///< Manual savegame.
  GOG_SAVEGAME_TYPE_POINT_OF_NO_RETURN, ///< Savegame at the point of no return.
  GOG_SAVEGAME_TYPE_ENDGAME_SAVE, ///< Savegame at the end of the game.
  GOG_SAVEGAME_TYPE_CHECKPOINT ///< Savegame at a certain in-game event, e.g. quest completion.
};

enum gog_GetFileListFailureReason {
  GOG_GETFILELIST_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_GETFILELIST_FAILURE_REASON_UNAUTHORIZED, ///< Authorization lost.
  GOG_GETFILELIST_FAILURE_REASON_FORBIDDEN, ///< No access to specified container.
  GOG_GETFILELIST_FAILURE_REASON_NOT_FOUND, ///< Container not found.
  GOG_GETFILELIST_FAILURE_REASON_UNAVAILABLE, ///< Service unavailable.
  GOG_GETFILELIST_FAILURE_REASON_ABORTED, ///< Request aborted.
  GOG_GETFILELIST_FAILURE_REASON_CONNECTION_FAILURE ///< Unable to communicate with backend services.
};

enum gog_GetFileFailureReason {
  GOG_GETFILE_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_GETFILE_FAILURE_REASON_UNAUTHORIZED, ///< Authorization lost.
  GOG_GETFILE_FAILURE_REASON_FORBIDDEN, ///< No access to specified container.
  GOG_GETFILE_FAILURE_REASON_NOT_FOUND, ///< Container not found.
  GOG_GETFILE_FAILURE_REASON_UNAVAILABLE, ///< Service unavailable.
  GOG_GETFILE_FAILURE_REASON_ABORTED, ///< Request aborted.
  GOG_GETFILE_FAILURE_REASON_CONNECTION_FAILURE, ///< Unable to communicate with backend services.
  GOG_GETFILE_FAILURE_REASON_BUFFER_TOO_SMALL, ///< Buffer passed to ICloudStorage::GetFile() is too small to fit the file content.
  GOG_GETFILE_FAILURE_REASON_WRITE_FUNC_ERROR ///< Function writeFunc passed to ICloudStorage::GetFile() returned an error.
};

enum gog_PutFileFailureReason {
  GGOG_PUTFILE_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GGOG_PUTFILE_FAILURE_REASON_UNAUTHORIZED, ///< Authorization lost.
  GGOG_PUTFILE_FAILURE_REASON_FORBIDDEN, ///< No access to specified container.
  GGOG_PUTFILE_FAILURE_REASON_UNAVAILABLE, ///< Service unavailable.
  GGOG_PUTFILE_FAILURE_REASON_ABORTED, ///< Request aborted.
  GGOG_PUTFILE_FAILURE_REASON_CONNECTION_FAILURE, ///< Unable to communicate with backend services.
  GGOG_PUTFILE_FAILURE_REASON_READ_FUNC_ERROR, ///< Function readFunc passed to ICloudStorage::PutFile() returned an error.
  GGOG_PUTFILE_FAILURE_REASON_QUOTA_EXCEEDED ///< Unable to upload the file because there is no free space on cloud storage.
};

enum gog_DeleteFileFailureReason {
  GOG_DELETEFILE_FAILURE_REASON_UNDEFINED, ///< Unspecified error.
  GOG_DELETEFILE_FAILURE_REASON_UNAUTHORIZED, ///< Authorization lost.
  GOG_DELETEFILE_FAILURE_REASON_FORBIDDEN, ///< No access to specified container.
  GOG_DELETEFILE_FAILURE_REASON_NOT_FOUND, ///< Container not found.
  GOG_DELETEFILE_FAILURE_REASON_UNAVAILABLE, ///< Service unavailable.
  GOG_DELETEFILE_FAILURE_REASON_ABORTED, ///< Request aborted.
  GOG_DELETEFILE_FAILURE_REASON_CONNECTION_FAILURE, ///< Unable to communicate with backend services.
  GOG_DELETEFILE_FAILURE_REASON_CONFLICT /// < Expected hash parameter doesn't match the hash in the container.
};

enum gog_ReadPhase {
  GOG_CHECKSUM_CALCULATING, ///< Checksum calculating phase.
  GOG_UPLOADING ///< Uploading phase.
};

typedef int (*gog_WriteFunc)(void* userParam, const char* data, int size);
typedef int (*gog_ReadFunc)(void* userParam, char* data, int size);
typedef int (*gog_RewindFunc)(void* userParam, enum gog_ReadPhase phase);

#define GOG_MIN_HASH_BUFFER_SIZE 33;

void gog_ICloudStorage_GetFileList(gog_Interface cloudstorage, const char* container, gog_Interface listener);
char* gog_ICloudStorage_GetFileNameByIndex(gog_Interface cloudstorage, uint32_t index);
uint32_t gog_ICloudStorage_GetFileSizeByIndex(gog_Interface cloudstorage, uint32_t index);
uint32_t gog_ICloudStorage_GetFileTimestampByIndex(gog_Interface cloudstorage, uint32_t index);
char* gog_ICloudStorage_GetFileHashByIndex(gog_Interface cloudstorage, uint32_t index);
void gog_ICloudStorage_GetFile_Callback(gog_Interface cloudstorage, const char* container, const char* name, void* userParam, gog_WriteFunc writeFunc, gog_Interface listener);
void gog_ICloudStorage_GetFile(gog_Interface cloudstorage, const char* container, const char* name, void* dataBuffer, uint32_t bufferLength, gog_Interface listener);
void gog_ICloudStorage_GetFileMetadata(gog_Interface cloudstorage, const char* container, const char* name, gog_Interface listener);
void gog_ICloudStorage_PutFile_Callback(gog_Interface cloudstorage, const char* container, const char* name, void* userParam, gog_ReadFunc readFunc, gog_RewindFunc rewindFunc, gog_Interface listener, enum gog_SavegameType savegameType, uint32_t timeStamp,const char* hash);
void gog_ICloudStorage_PutFile(gog_Interface cloudstorage, const char* container, const char* name, const void* buffer, uint32_t bufferLength, gog_Interface listener, enum gog_SavegameType savegameType, uint32_t timeStamp, const char* hash);
void gog_ICloudStorage_CalculateHash_Callback(gog_Interface cloudstorage, void* userParam, gog_ReadFunc readFunc, gog_RewindFunc rewindFunc, char* hashBuffer, uint32_t hashBufferSize);
void gog_ICloudStorage_CalculateHash(gog_Interface cloudstorage, const void* buffer, uint32_t bufferLength, char* hashBuffer, uint32_t hashBufferSize);
void gog_ICloudStorage_DeleteFile(gog_Interface cloudstorage, const char* container, const char* name, gog_Interface listener, const char* expectedHash);
void gog_ICloudStorage_OpenSavegame(gog_Interface cloudstorage);
void gog_ICloudStorage_CloseSavegame(gog_Interface cloudstorage);


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

enum gog_FailureReason {
  GOG_BASIC_FAILURE_REASON_UNDEFINED, 
  GOG_BASIC_FAILURE_REASON_CONNECTION_FAILURE 
};

struct gog_listener {
  void *userptr;
  void (*OnAuthSuccess)(void *userptr);
  void (*OnAuthFailure)(void *userptr, enum gog_AuthFailureReason reason);
  void (*OnAuthLost)(void *userptr);
  void (*OnOtherSessionStarted)(void *userptr);
  void (*OnOperationalStateChanged)(void *userptr, enum gog_OperationalState state);
  void (*OnUserDataUpdated)(void *userptr);
  void (*OnSpecificUserDataUpdated)(void *userptr, gog_ID user);
  void (*OnEncryptedAppTicketRetrieveSuccess)(void *userptr);
  void (*OnEncryptedAppTicketRetrieveFailure)(void *userptr, enum gog_FailureReason failureReason);
  void (*OnAccessTokenChanged)(void *userptr);
  void (*OnPlayFabCreateOpenIDConnectionSuccess)(void *userptr, bool connectionAlreadyExists);
  void (*OnPlayFabCreateOpenIDConnectionFailure)(void *userptr, enum gog_FailureReason failureReason);
  void (*OnPersonaDataChanged)(void *userptr, gog_ID userID, uint32_t personaStateChange);
  void (*OnUserInformationRetrieveSuccess)(void *userptr, gog_ID userID);
  void (*OnUserInformationRetrieveFailure)(void *userptr, gog_ID userID, enum gog_FailureReason failureReason);
  void (*OnFriendListRetrieveSuccess)(void *userptr);
  void (*OnFriendListRetrieveFailure)(void *userptr, enum gog_FailureReason failureReason);
  void (*OnFriendInvitationSendSuccess)(void *userptr, gog_ID userID);
  void (*OnFriendInvitationSendFailure)(void *userptr, gog_ID userID, enum gog_FriendInvitationFailureReason failureReason);
  void (*OnFriendInvitationListRetrieveSuccess)(void *userptr);
  void (*OnFriendInvitationListRetrieveFailure)(void *userptr, enum gog_FailureReason failureReason);
  void (*OnSentFriendInvitationListRetrieveSuccess)(void *userptr);
  void (*OnSentFriendInvitationListRetrieveFailure)(void *userptr, enum gog_FailureReason failureReason);
  void (*OnFriendInvitationReceived)(void *userptr, gog_ID userID, uint32_t sendTime);
  void (*OnFriendInvitationRespondToSuccess)(void *userptr, gog_ID userID, bool accept);
  void (*OnFriendInvitationRespondToFailure)(void *userptr, gog_ID userID, enum gog_FriendInvitationRespondFailureReason failureReason);
  void (*OnFriendAdded)(void *userptr, gog_ID userID, enum gog_InvitationDirection invitationDirection);
  void (*OnFriendDeleteSuccess)(void *userptr, gog_ID userID);
  void (*OnFriendDeleteFailure)(void *userptr, gog_ID userID, enum gog_FailureReason failureReason);
  void (*OnRichPresenceChangeSuccess)(void *userptr);
  void (*OnRichPresenceChangeFailure)(void *userptr, enum gog_FailureReason failureReason);
  void (*OnRichPresenceUpdated)(void *userptr, gog_ID userID);
  void (*OnRichPresenceRetrieveSuccess)(void *userptr, gog_ID userID);
  void (*OnRichPresenceRetrieveFailure)(void *userptr, gog_ID userID, enum gog_FailureReason failureReason);
  void (*OnGameJoinRequested)(void *userptr, gog_ID userID, const char* connectionString);
  void (*OnGameInvitationReceived)(void *userptr, gog_ID userID, const char* connectionString);
  void (*OnInvitationSendSuccess)(void *userptr, gog_ID userID, const char* connectionString);
  void (*OnInvitationSendFailure)(void *userptr, gog_ID userID, const char* connectionString, enum gog_SendInvitationFailureReason failureReason);
  void (*OnUserFindSuccess)(void *userptr, const char* userSpecifier, gog_ID userID);
  void (*OnUserFindFailure)(void *userptr, const char* userSpecifier, enum gog_UserFindFailureReason failureReason);
  void (*OnChatRoomWithUserRetrieveSuccess)(void *userptr, gog_ID userID, gog_ChatRoomID chatRoomID);
  void (*OnChatRoomWithUserRetrieveFailure)(void *userptr, gog_ID userID, enum gog_ChatUserRetrieveFailureReason failureReason);
  void (*OnChatRoomMessageSendSuccess)(void *userptr, gog_ChatRoomID chatRoomID, uint32_t sentMessageIndex, gog_ChatMessageID messageID, uint32_t sendTime);
  void (*OnChatRoomMessageSendFailure)(void *userptr, gog_ChatRoomID chatRoomID, uint32_t sentMessageIndex, enum gog_ChatMessageSendFailureReason failureReason);
  void (*OnChatRoomMessagesReceived)(void *userptr, gog_ChatRoomID chatRoomID, uint32_t messageCount, uint32_t longestMessageLenght);
  void (*OnChatRoomMessagesRetrieveSuccess)(void *userptr, gog_ChatRoomID chatRoomID, uint32_t messageCount, uint32_t longestMessageLenght);
  void (*OnChatRoomMessagesRetrieveFailure)(void *userptr, gog_ChatRoomID chatRoomID, enum gog_ChatMessageRetrieveFailureReason failureReason);
  void (*OnLobbyList)(void *userptr, uint32_t lobbyCount, enum gog_LobbyListResult result);
  void (*OnLobbyCreated)(void *userptr, gog_ID lobbyID, enum gog_LobbyCreateResult result);
  void (*OnLobbyEntered)(void *userptr, gog_ID lobbyID, enum gog_LobbyEnterResult result);
  void (*OnLobbyLeft)(void *userptr, gog_ID lobbyID, enum gog_LobbyLeaveReason leaveReason);
  void (*OnLobbyDataUpdated)(void *userptr, gog_ID lobbyID, gog_ID memberID);
  void (*OnLobbyDataUpdateSuccess)(void *userptr, gog_ID lobbyID);
  void (*OnLobbyDataUpdateFailure)(void *userptr, gog_ID lobbyID, enum gog_LobbyDataUpdateFailureReason failureReason);
  void (*OnLobbyMemberDataUpdateSuccess)(void *userptr, gog_ID lobbyID, gog_ID memberID);
  void (*OnLobbyMemberDataUpdateFailure)(void *userptr, gog_ID lobbyID, gog_ID memberID, enum gog_LobbyDataUpdateFailureReason failureReason);
  void (*OnLobbyDataRetrieveFailure)(void *userptr, gog_ID lobbyID, enum gog_LobbyDataRetrieveFailureReason failureReason);
  void (*OnLobbyMemberStateChanged)(void *userptr, gog_ID lobbyID, gog_ID memberID, enum gog_LobbyMemberStateChange memberStateChange);
  void (*OnLobbyOwnerChanged)(void *userptr, gog_ID lobbyID, gog_ID newOwnerID);
  void (*OnLobbyMessageReceived)(void *userptr, gog_ID lobbyID, gog_ID senderID, uint32_t messageID, uint32_t messageLength);
  void (*OnP2PPacketAvailable)(void *userptr, uint32_t msgSize, uint8_t channel);
  void (*OnNatTypeDetectionSuccess)(void *userptr, enum gog_NatType natType);
  void (*OnNatTypeDetectionFailure)(void *userptr);
  void (*OnUserStatsAndAchievementsRetrieveSuccess)(void *userptr, gog_ID userID);
  void (*OnUserStatsAndAchievementsRetrieveFailure)(void *userptr, gog_ID userID, enum gog_FailureReason failureReason);
  void (*OnUserStatsAndAchievementsStoreSuccess)(void *userptr);
  void (*OnUserStatsAndAchievementsStoreFailure)(void *userptr, enum gog_FailureReason failureReason);
  void (*OnAchievementUnlocked)(void *userptr, const char* name);
  void (*OnLeaderboardsRetrieveSuccess)(void *userptr);
  void (*OnLeaderboardsRetrieveFailure)(void *userptr, enum gog_FailureReason failureReason);
  void (*OnLeaderboardEntriesRetrieveSuccess)(void *userptr, const char* name, uint32_t entryCount);
  void (*OnLeaderboardEntriesRetrieveFailure)(void *userptr, const char* name, enum gog_FailureReason failureReason);
  void (*OnLeaderboardScoreUpdateSuccess)(void *userptr, const char* name, int32_t score, uint32_t oldRank, uint32_t newRank);
  void (*OnLeaderboardScoreUpdateFailure)(void *userptr, const char* name, int32_t score, enum gog_LeaderboardScoreUpdateFailureReason failureReason);
  void (*OnLeaderboardRetrieveSuccess)(void *userptr, const char* name);
  void (*OnLeaderboardRetrieveFailure)(void *userptr, const char* name, enum gog_FailureReason failureReason);
  void (*OnUserTimePlayedRetrieveSuccess)(void *userptr, gog_ID userID);
  void (*OnUserTimePlayedRetrieveFailure)(void *userptr, gog_ID userID, enum gog_FailureReason failureReason);
  void (*OnOverlayVisibilityChanged)(void *userptr, bool overlayVisible);
  void (*OnOverlayStateChanged)(void *userptr, enum gog_OverlayState overlayState);
  void (*OnNotificationReceived)(void *userptr, gog_NotificationID notificationID, uint32_t typeLength, uint32_t contentSize);
  void (*OnConnectionStateChange)(void *userptr, enum gog_ServicesConnectionState connectionState);
  void (*OnDlcCheckSuccess)(void *userptr, gog_ProductID productId, bool isOwned);
  void (*OnDlcCheckFailure)(void *userptr, gog_ProductID productId, enum gog_DlcCheckFailureReason failureReason);
  void (*OnFileShareSuccess)(void *userptr, const char* fileName, gog_SharedFileID sharedFileID);
  void (*OnFileShareFailure)(void *userptr, const char* fileName, enum gog_FailureReason failureReason);
  void (*OnSharedFileDownloadSuccess)(void *userptr, gog_SharedFileID sharedFileID, const char* fileName);
  void (*OnSharedFileDownloadFailure)(void *userptr, gog_SharedFileID sharedFileID, enum gog_FailureReason failureReason);
  void (*OnConnectionOpenSuccess)(void *userptr, const char* connectionString, gog_ConnectionID connectionID);
  void (*OnConnectionOpenFailure)(void *userptr, const char* connectionString, enum gog_ConnectionOpenFailureReason failureReason);
  void (*OnConnectionClosed)(void *userptr, gog_ConnectionID connectionID, enum gog_CloseReason closeReason);
  void (*OnConnectionDataReceived)(void *userptr, gog_ConnectionID connectionID, uint32_t dataSize);
  void (*OnTelemetryEventSendSuccess)(void *userptr, const char* eventType, uint32_t sentEventIndex);
  void (*OnTelemetryEventSendFailure)(void *userptr, const char* eventType, uint32_t sentEventIndex, enum gog_TelemetryEventFailureReason failureReason);
  void (*OnGetFileListSuccess)(void *userptr, uint32_t fileCount, uint32_t quota, uint32_t quotaUsed);
  void (*OnGetFileListFailure)(void *userptr, enum gog_GetFileListFailureReason failureReason);
  void (*OnGetFileSuccess)(void *userptr, const char* container, const char* name, uint32_t fileSize, enum gog_SavegameType savegameType, const char* savegameID);
  void (*OnGetFileFailure)(void *userptr, const char* container, const char* name, enum gog_GetFileFailureReason failureReason);
  void (*OnPutFileSuccess)(void *userptr, const char* container, const char* name);
  void (*OnPutFileFailure)(void *userptr, const char* container, const char* name, enum gog_PutFileFailureReason failureReason);
  void (*OnDeleteFileSuccess)(void *userptr, const char* container, const char* name);
  void (*OnDeleteFileFailure)(void *userptr, const char* container, const char* name, enum gog_DeleteFileFailureReason failureReason);
};

void gog_IListenerRegistrar_Register(gog_Interface registrar, enum gog_ListenerType type, gog_Interface listener);
void gog_IListenerRegistrar_Unregister(gog_Interface registrar, enum gog_ListenerType type, gog_Interface listener);
gog_Interface gog_ListenerRegistrar();
gog_Interface gog_GameServerListenerRegistrar();
gog_Interface gog_MakeListener(struct gog_listener *listener);
void gog_FreeListener(gog_Interface listener);

#ifdef __cplusplus
}
#endif
#endif
