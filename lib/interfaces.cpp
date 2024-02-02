#include <galaxy/GalaxyApi.h>
#include "gog.h"

using namespace galaxy::api;
typedef galaxy::api::ICloudStorage::RewindFunc RewindFunc;
typedef galaxy::api::ICloudStorage::ReadFunc ReadFunc;
typedef galaxy::api::ICloudStorage::WriteFunc WriteFunc;
bool gog_IUser_SignedIn(gog_Interface user){
  return (bool)((galaxy::api::IUser*)user)->SignedIn();
}

gog_ID gog_IUser_GetGalaxyID(gog_Interface user){
  return (gog_ID)((galaxy::api::IUser*)user)->GetGalaxyID().ToUint64();
}

void gog_IUser_SignInCredentials(gog_Interface user, char* login, char* password, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SignInCredentials((const char*)login, (const char*)password, (IAuthListener* const)listener);
}

void gog_IUser_SignInToken(gog_Interface user, char* refreshToken, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SignInToken((const char*)refreshToken, (IAuthListener* const)listener);
}

void gog_IUser_SignInLauncher(gog_Interface user, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SignInLauncher((IAuthListener* const)listener);
}

void gog_IUser_SignInSteam(gog_Interface user, void* steamAppTicket, uint32_t steamAppTicketSize, char* personaName, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SignInSteam((const void*)steamAppTicket, (uint32_t)steamAppTicketSize, (const char*)personaName, (IAuthListener* const)listener);
}

void gog_IUser_SignInGalaxy(gog_Interface user, bool requireOnline, uint32_t timeout, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SignInGalaxy((bool)requireOnline, (uint32_t)timeout, (IAuthListener* const)listener);
}

void gog_IUser_SignInPS4(gog_Interface user, char* ps4ClientID, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SignInPS4((const char*)ps4ClientID, (IAuthListener* const)listener);
}

void gog_IUser_SignInXB1(gog_Interface user, char* xboxOneUserID, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SignInXB1((const char*)xboxOneUserID, (IAuthListener* const)listener);
}

void gog_IUser_SignInXbox(gog_Interface user, uint64_t xboxID, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SignInXbox((uint64_t)xboxID, (IAuthListener* const)listener);
}

void gog_IUser_SignInXBLive(gog_Interface user, char* token, char* signature, char* marketplaceID, char* locale, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SignInXBLive((const char*)token, (const char*)signature, (const char*)marketplaceID, (const char*)locale, (IAuthListener* const)listener);
}

void gog_IUser_SignInAnonymous(gog_Interface user, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SignInAnonymous((IAuthListener* const)listener);
}

void gog_IUser_SignInAnonymousTelemetry(gog_Interface user, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SignInAnonymousTelemetry((IAuthListener* const)listener);
}

void gog_IUser_SignInServerKey(gog_Interface user, char* serverKey, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SignInServerKey((const char*)serverKey, (IAuthListener* const)listener);
}

void gog_IUser_SignInAuthorizationCode(gog_Interface user, char* authorizationCode, char* redirectURI, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SignInAuthorizationCode((const char*)authorizationCode, (const char*)redirectURI, (IAuthListener* const)listener);
}

void gog_IUser_SignOut(gog_Interface user){
  ((galaxy::api::IUser*)user)->SignOut();
}

void gog_IUser_RequestUserData(gog_Interface user, gog_ID userID, gog_Interface listener){
  ((galaxy::api::IUser*)user)->RequestUserData((GalaxyID)userID, (ISpecificUserDataListener* const)listener);
}

bool gog_IUser_IsUserDataAvailable(gog_Interface user, gog_ID userID){
  return (bool)((galaxy::api::IUser*)user)->IsUserDataAvailable((GalaxyID)userID);
}

char* gog_IUser_GetUserData(gog_Interface user, char* key, gog_ID userID){
  return (char*)((galaxy::api::IUser*)user)->GetUserData((const char*)key, (GalaxyID)userID);
}

void gog_IUser_GetUserDataCopy(gog_Interface user, char* key, char* buffer, uint32_t bufferLength, gog_ID userID){
  ((galaxy::api::IUser*)user)->GetUserDataCopy((const char*)key, (char*)buffer, (uint32_t)bufferLength, (GalaxyID)userID);
}

void gog_IUser_SetUserData(gog_Interface user, char* key, char* value, gog_Interface listener){
  ((galaxy::api::IUser*)user)->SetUserData((const char*)key, (const char*)value, (ISpecificUserDataListener* const)listener);
}

uint32_t gog_IUser_GetUserDataCount(gog_Interface user, gog_ID userID){
  return (uint32_t)((galaxy::api::IUser*)user)->GetUserDataCount((GalaxyID)userID);
}

bool gog_IUser_GetUserDataByIndex(gog_Interface user, uint32_t index, char* key, uint32_t keyLength, char* value, uint32_t valueLength, gog_ID userID){
  return (bool)((galaxy::api::IUser*)user)->GetUserDataByIndex((uint32_t)index, (char*)key, (uint32_t)keyLength, (char*)value, (uint32_t)valueLength, (GalaxyID)userID);
}

void gog_IUser_DeleteUserData(gog_Interface user, char* key, gog_Interface listener){
  ((galaxy::api::IUser*)user)->DeleteUserData((const char*)key, (ISpecificUserDataListener* const)listener);
}

bool gog_IUser_IsLoggedOn(gog_Interface user){
  return (bool)((galaxy::api::IUser*)user)->IsLoggedOn();
}

void gog_IUser_RequestEncryptedAppTicket(gog_Interface user, void* data, uint32_t dataSize, gog_Interface listener){
  ((galaxy::api::IUser*)user)->RequestEncryptedAppTicket((const void*)data, (uint32_t)dataSize, (IEncryptedAppTicketListener* const)listener);
}

void gog_IUser_GetEncryptedAppTicket(gog_Interface user, void* encryptedAppTicket, uint32_t maxEncryptedAppTicketSize, uint32_t* currentEncryptedAppTicketSize){
  ((galaxy::api::IUser*)user)->GetEncryptedAppTicket((void*)encryptedAppTicket, (uint32_t)maxEncryptedAppTicketSize, (uint32_t&)currentEncryptedAppTicketSize);
}

void gog_IUser_CreateOpenIDConnection(gog_Interface user, char* secretKey, char* titleID, char* connectionID, bool ignoreNonce, gog_Interface listener){
  ((galaxy::api::IUser*)user)->CreateOpenIDConnection((const char*)secretKey, (const char*)titleID, (const char*)connectionID, (bool)ignoreNonce, (IPlayFabCreateOpenIDConnectionListener* const)listener);
}

void gog_IUser_LoginWithOpenIDConnect(gog_Interface user, char* titleID, char* connectionID, char* idToken, bool createAccount, char* encryptedRequest, char* playerSecret, gog_Interface listener){
  ((galaxy::api::IUser*)user)->LoginWithOpenIDConnect((const char*)titleID, (const char*)connectionID, (const char*)idToken, (bool)createAccount, (const char*)encryptedRequest, (const char*)playerSecret, (IPlayFabLoginWithOpenIDConnectListener* const)listener);
}

gog_SessionID gog_IUser_GetSessionID(gog_Interface user){
  return (gog_SessionID)((galaxy::api::IUser*)user)->GetSessionID();
}

char* gog_IUser_GetAccessToken(gog_Interface user){
  return (char*)((galaxy::api::IUser*)user)->GetAccessToken();
}

void gog_IUser_GetAccessTokenCopy(gog_Interface user, char* buffer, uint32_t bufferLength){
  ((galaxy::api::IUser*)user)->GetAccessTokenCopy((char*)buffer, (uint32_t)bufferLength);
}

char* gog_IUser_GetRefreshToken(gog_Interface user){
  return (char*)((galaxy::api::IUser*)user)->GetRefreshToken();
}

void gog_IUser_GetRefreshTokenCopy(gog_Interface user, char* buffer, uint32_t bufferLength){
  ((galaxy::api::IUser*)user)->GetRefreshTokenCopy((char*)buffer, (uint32_t)bufferLength);
}

char* gog_IUser_GetIDToken(gog_Interface user){
  return (char*)((galaxy::api::IUser*)user)->GetIDToken();
}

void gog_IUser_GetIDTokenCopy(gog_Interface user, char* buffer, uint32_t bufferLength){
  ((galaxy::api::IUser*)user)->GetIDTokenCopy((char*)buffer, (uint32_t)bufferLength);
}

bool gog_IUser_ReportInvalidAccessToken(gog_Interface user, char* accessToken, char* info){
  return (bool)((galaxy::api::IUser*)user)->ReportInvalidAccessToken((const char*)accessToken, (const char*)info);
}

gog_AvatarCriteria gog_IFriends_GetDefaultAvatarCriteria(gog_Interface _friend){
  return (gog_AvatarCriteria)((galaxy::api::IFriends*)_friend)->GetDefaultAvatarCriteria();
}

void gog_IFriends_SetDefaultAvatarCriteria(gog_Interface _friend, gog_AvatarCriteria defaultAvatarCriteria){
  ((galaxy::api::IFriends*)_friend)->SetDefaultAvatarCriteria((AvatarCriteria)defaultAvatarCriteria);
}

void gog_IFriends_RequestUserInformation(gog_Interface _friend, gog_ID userID, gog_AvatarCriteria avatarCriteria, gog_Interface listener){
  ((galaxy::api::IFriends*)_friend)->RequestUserInformation((GalaxyID)userID, (AvatarCriteria)avatarCriteria, (IUserInformationRetrieveListener* const)listener);
}

bool gog_IFriends_IsUserInformationAvailable(gog_Interface _friend, gog_ID userID){
  return (bool)((galaxy::api::IFriends*)_friend)->IsUserInformationAvailable((GalaxyID)userID);
}

char* gog_IFriends_GetPersonaName(gog_Interface _friend){
  return (char*)((galaxy::api::IFriends*)_friend)->GetPersonaName();
}

void gog_IFriends_GetPersonaNameCopy(gog_Interface _friend, char* buffer, uint32_t bufferLength){
  ((galaxy::api::IFriends*)_friend)->GetPersonaNameCopy((char*)buffer, (uint32_t)bufferLength);
}

enum gog_PersonaState gog_IFriends_GetPersonaState(gog_Interface _friend){
  return (enum gog_PersonaState)((galaxy::api::IFriends*)_friend)->GetPersonaState();
}

char* gog_IFriends_GetFriendPersonaName(gog_Interface _friend, gog_ID userID){
  return (char*)((galaxy::api::IFriends*)_friend)->GetFriendPersonaName((GalaxyID)userID);
}

void gog_IFriends_GetFriendPersonaNameCopy(gog_Interface _friend, gog_ID userID, char* buffer, uint32_t bufferLength){
  ((galaxy::api::IFriends*)_friend)->GetFriendPersonaNameCopy((GalaxyID)userID, (char*)buffer, (uint32_t)bufferLength);
}

enum gog_PersonaState gog_IFriends_GetFriendPersonaState(gog_Interface _friend, gog_ID userID){
  return (enum gog_PersonaState)((galaxy::api::IFriends*)_friend)->GetFriendPersonaState((GalaxyID)userID);
}

char* gog_IFriends_GetFriendAvatarUrl(gog_Interface _friend, gog_ID userID, enum gog_AvatarType avatarType){
  return (char*)((galaxy::api::IFriends*)_friend)->GetFriendAvatarUrl((GalaxyID)userID, (AvatarType)avatarType);
}

void gog_IFriends_GetFriendAvatarUrlCopy(gog_Interface _friend, gog_ID userID, enum gog_AvatarType avatarType, char* buffer, uint32_t bufferLength){
  ((galaxy::api::IFriends*)_friend)->GetFriendAvatarUrlCopy((GalaxyID)userID, (AvatarType)avatarType, (char*)buffer, (uint32_t)bufferLength);
}

uint32_t gog_IFriends_GetFriendAvatarImageID(gog_Interface _friend, gog_ID userID, enum gog_AvatarType avatarType){
  return (uint32_t)((galaxy::api::IFriends*)_friend)->GetFriendAvatarImageID((GalaxyID)userID, (AvatarType)avatarType);
}

void gog_IFriends_GetFriendAvatarImageRGBA(gog_Interface _friend, gog_ID userID, enum gog_AvatarType avatarType, void* buffer, uint32_t bufferLength){
  ((galaxy::api::IFriends*)_friend)->GetFriendAvatarImageRGBA((GalaxyID)userID, (AvatarType)avatarType, (void*)buffer, (uint32_t)bufferLength);
}

bool gog_IFriends_IsFriendAvatarImageRGBAAvailable(gog_Interface _friend, gog_ID userID, enum gog_AvatarType avatarType){
  return (bool)((galaxy::api::IFriends*)_friend)->IsFriendAvatarImageRGBAAvailable((GalaxyID)userID, (AvatarType)avatarType);
}

void gog_IFriends_RequestFriendList(gog_Interface _friend, gog_Interface listener){
  ((galaxy::api::IFriends*)_friend)->RequestFriendList((IFriendListListener* const)listener);
}

bool gog_IFriends_IsFriend(gog_Interface _friend, gog_ID userID){
  return (bool)((galaxy::api::IFriends*)_friend)->IsFriend((GalaxyID)userID);
}

uint32_t gog_IFriends_GetFriendCount(gog_Interface _friend){
  return (uint32_t)((galaxy::api::IFriends*)_friend)->GetFriendCount();
}

gog_ID gog_IFriends_GetFriendByIndex(gog_Interface _friend, uint32_t index){
  return (gog_ID)((galaxy::api::IFriends*)_friend)->GetFriendByIndex((uint32_t)index).ToUint64();
}

void gog_IFriends_SendFriendInvitation(gog_Interface _friend, gog_ID userID, gog_Interface listener){
  ((galaxy::api::IFriends*)_friend)->SendFriendInvitation((GalaxyID)userID, (IFriendInvitationSendListener* const)listener);
}

void gog_IFriends_RequestFriendInvitationList(gog_Interface _friend, gog_Interface listener){
  ((galaxy::api::IFriends*)_friend)->RequestFriendInvitationList((IFriendInvitationListRetrieveListener* const)listener);
}

void gog_IFriends_RequestSentFriendInvitationList(gog_Interface _friend, gog_Interface listener){
  ((galaxy::api::IFriends*)_friend)->RequestSentFriendInvitationList((ISentFriendInvitationListRetrieveListener* const)listener);
}

uint32_t gog_IFriends_GetFriendInvitationCount(gog_Interface _friend){
  return (uint32_t)((galaxy::api::IFriends*)_friend)->GetFriendInvitationCount();
}

void gog_IFriends_GetFriendInvitationByIndex(gog_Interface _friend, uint32_t index, gog_ID* userID, uint32_t* sendTime){
  ((galaxy::api::IFriends*)_friend)->GetFriendInvitationByIndex((uint32_t)index, (GalaxyID&)userID, (uint32_t&)sendTime);
}

void gog_IFriends_RespondToFriendInvitation(gog_Interface _friend, gog_ID userID, bool accept, gog_Interface listener){
  ((galaxy::api::IFriends*)_friend)->RespondToFriendInvitation((GalaxyID)userID, (bool)accept, (IFriendInvitationRespondToListener* const)listener);
}

void gog_IFriends_DeleteFriend(gog_Interface _friend, gog_ID userID, gog_Interface listener){
  ((galaxy::api::IFriends*)_friend)->DeleteFriend((GalaxyID)userID, (IFriendDeleteListener* const)listener);
}

void gog_IFriends_SetRichPresence(gog_Interface _friend, char* key, char* value, gog_Interface listener){
  ((galaxy::api::IFriends*)_friend)->SetRichPresence((const char*)key, (const char*)value, (IRichPresenceChangeListener* const)listener);
}

void gog_IFriends_DeleteRichPresence(gog_Interface _friend, char* key, gog_Interface listener){
  ((galaxy::api::IFriends*)_friend)->DeleteRichPresence((const char*)key, (IRichPresenceChangeListener* const)listener);
}

void gog_IFriends_ClearRichPresence(gog_Interface _friend, gog_Interface listener){
  ((galaxy::api::IFriends*)_friend)->ClearRichPresence((IRichPresenceChangeListener* const)listener);
}

void gog_IFriends_RequestRichPresence(gog_Interface _friend, gog_ID userID, gog_Interface listener){
  ((galaxy::api::IFriends*)_friend)->RequestRichPresence((GalaxyID)userID, (IRichPresenceRetrieveListener* const)listener);
}

char* gog_IFriends_GetRichPresence(gog_Interface _friend, char* key, gog_ID userID){
  return (char*)((galaxy::api::IFriends*)_friend)->GetRichPresence((const char*)key, (GalaxyID)userID);
}

void gog_IFriends_GetRichPresenceCopy(gog_Interface _friend, char* key, char* buffer, uint32_t bufferLength, gog_ID userID){
  ((galaxy::api::IFriends*)_friend)->GetRichPresenceCopy((const char*)key, (char*)buffer, (uint32_t)bufferLength, (GalaxyID)userID);
}

uint32_t gog_IFriends_GetRichPresenceCount(gog_Interface _friend, gog_ID userID){
  return (uint32_t)((galaxy::api::IFriends*)_friend)->GetRichPresenceCount((GalaxyID)userID);
}

void gog_IFriends_GetRichPresenceByIndex(gog_Interface _friend, uint32_t index, char* key, uint32_t keyLength, char* value, uint32_t valueLength, gog_ID userID){
  ((galaxy::api::IFriends*)_friend)->GetRichPresenceByIndex((uint32_t)index, (char*)key, (uint32_t)keyLength, (char*)value, (uint32_t)valueLength, (GalaxyID)userID);
}

char* gog_IFriends_GetRichPresenceKeyByIndex(gog_Interface _friend, uint32_t index, gog_ID userID){
  return (char*)((galaxy::api::IFriends*)_friend)->GetRichPresenceKeyByIndex((uint32_t)index, (GalaxyID)userID);
}

void gog_IFriends_GetRichPresenceKeyByIndexCopy(gog_Interface _friend, uint32_t index, char* buffer, uint32_t bufferLength, gog_ID userID){
  ((galaxy::api::IFriends*)_friend)->GetRichPresenceKeyByIndexCopy((uint32_t)index, (char*)buffer, (uint32_t)bufferLength, (GalaxyID)userID);
}

void gog_IFriends_ShowOverlayInviteDialog(gog_Interface _friend, char* connectionString){
  ((galaxy::api::IFriends*)_friend)->ShowOverlayInviteDialog((const char*)connectionString);
}

void gog_IFriends_SendInvitation(gog_Interface _friend, gog_ID userID, char* connectionString, gog_Interface listener){
  ((galaxy::api::IFriends*)_friend)->SendInvitation((GalaxyID)userID, (const char*)connectionString, (ISendInvitationListener* const)listener);
}

void gog_IFriends_FindUser(gog_Interface _friend, char* userSpecifier, gog_Interface listener){
  ((galaxy::api::IFriends*)_friend)->FindUser((const char*)userSpecifier, (IUserFindListener* const)listener);
}

bool gog_IFriends_IsUserInTheSameGame(gog_Interface _friend, gog_ID userID){
  return (bool)((galaxy::api::IFriends*)_friend)->IsUserInTheSameGame((GalaxyID)userID);
}

void gog_IChat_RequestChatRoomWithUser(gog_Interface chat, gog_ID userID, gog_Interface listener){
  ((galaxy::api::IChat*)chat)->RequestChatRoomWithUser((GalaxyID)userID, (IChatRoomWithUserRetrieveListener* const)listener);
}

void gog_IChat_RequestChatRoomMessages(gog_Interface chat, gog_ChatRoomID chatRoomID, uint32_t limit, gog_ChatMessageID referenceMessageID, gog_Interface listener){
  ((galaxy::api::IChat*)chat)->RequestChatRoomMessages((ChatRoomID)chatRoomID, (uint32_t)limit, (ChatMessageID)referenceMessageID, (IChatRoomMessagesRetrieveListener* const)listener);
}

uint32_t gog_IChat_SendChatRoomMessage(gog_Interface chat, gog_ChatRoomID chatRoomID, const char* msg, gog_Interface listener){
  return (uint32_t)((galaxy::api::IChat*)chat)->SendChatRoomMessage((ChatRoomID)chatRoomID, (const char*)msg, (IChatRoomMessageSendListener* const)listener);
}

uint32_t gog_IChat_GetChatRoomMessageByIndex(gog_Interface chat, uint32_t index, gog_ChatMessageID* messageID, enum gog_ChatMessageType* messageType, gog_ID* senderID, uint32_t* sendTime, char* buffer, uint32_t bufferLength){
  return (uint32_t)((galaxy::api::IChat*)chat)->GetChatRoomMessageByIndex((uint32_t)index, (ChatMessageID&)messageID, (ChatMessageType&)messageType, (GalaxyID&)senderID, (uint32_t&)sendTime, (char*)buffer, (uint32_t)bufferLength);
}

uint32_t gog_IChat_GetChatRoomMemberCount(gog_Interface chat, gog_ChatRoomID chatRoomID){
  return (uint32_t)((galaxy::api::IChat*)chat)->GetChatRoomMemberCount((ChatRoomID)chatRoomID);
}

gog_ID gog_IChat_GetChatRoomMemberUserIDByIndex(gog_Interface chat, gog_ChatRoomID chatRoomID, uint32_t index){
  return (gog_ID)((galaxy::api::IChat*)chat)->GetChatRoomMemberUserIDByIndex((ChatRoomID)chatRoomID, (uint32_t)index).ToUint64();
}

uint32_t gog_IChat_GetChatRoomUnreadMessageCount(gog_Interface chat, gog_ChatRoomID chatRoomID){
  return (uint32_t)((galaxy::api::IChat*)chat)->GetChatRoomUnreadMessageCount((ChatRoomID)chatRoomID);
}

void gog_IChat_MarkChatRoomAsRead(gog_Interface chat, gog_ChatRoomID chatRoomID){
  ((galaxy::api::IChat*)chat)->MarkChatRoomAsRead((ChatRoomID)chatRoomID);
}

void gog_IMatchmaking_CreateLobby(gog_Interface matchmaking, enum gog_LobbyType lobbyType, uint32_t maxMembers, bool joinable, enum gog_LobbyTopologyType lobbyTopologyType, gog_Interface created, gog_Interface entered){
  ((galaxy::api::IMatchmaking*)matchmaking)->CreateLobby((LobbyType)lobbyType, (uint32_t)maxMembers, (bool)joinable, (LobbyTopologyType)lobbyTopologyType, (ILobbyCreatedListener* const)created, (ILobbyEnteredListener* const)entered);
}

void gog_IMatchmaking_RequestLobbyList(gog_Interface matchmaking, bool allowFullLobbies, gog_Interface listener){
  ((galaxy::api::IMatchmaking*)matchmaking)->RequestLobbyList((bool)allowFullLobbies, (ILobbyListListener* const)listener);
}

void gog_IMatchmaking_AddRequestLobbyListResultCountFilter(gog_Interface matchmaking, uint32_t limit){
  ((galaxy::api::IMatchmaking*)matchmaking)->AddRequestLobbyListResultCountFilter((uint32_t)limit);
}

void gog_IMatchmaking_AddRequestLobbyListStringFilter(gog_Interface matchmaking, char* keyToMatch, char* valueToMatch, enum gog_LobbyComparisonType comparisonType){
  ((galaxy::api::IMatchmaking*)matchmaking)->AddRequestLobbyListStringFilter((const char*)keyToMatch, (const char*)valueToMatch, (LobbyComparisonType)comparisonType);
}

void gog_IMatchmaking_AddRequestLobbyListNumericalFilter(gog_Interface matchmaking, char* keyToMatch, int32_t valueToMatch, enum gog_LobbyComparisonType comparisonType){
  ((galaxy::api::IMatchmaking*)matchmaking)->AddRequestLobbyListNumericalFilter((const char*)keyToMatch, (int32_t)valueToMatch, (LobbyComparisonType)comparisonType);
}

void gog_IMatchmaking_AddRequestLobbyListNearValueFilter(gog_Interface matchmaking, char* keyToMatch, int32_t valueToBeCloseTo){
  ((galaxy::api::IMatchmaking*)matchmaking)->AddRequestLobbyListNearValueFilter((const char*)keyToMatch, (int32_t)valueToBeCloseTo);
}

gog_ID gog_IMatchmaking_GetLobbyByIndex(gog_Interface matchmaking, uint32_t index){
  return (gog_ID)((galaxy::api::IMatchmaking*)matchmaking)->GetLobbyByIndex((uint32_t)index).ToUint64();
}

void gog_IMatchmaking_JoinLobby(gog_Interface matchmaking, gog_ID lobbyID, gog_Interface listener){
  ((galaxy::api::IMatchmaking*)matchmaking)->JoinLobby((GalaxyID)lobbyID, (ILobbyEnteredListener* const)listener);
}

void gog_IMatchmaking_LeaveLobby(gog_Interface matchmaking, gog_ID lobbyID, gog_Interface listener){
  ((galaxy::api::IMatchmaking*)matchmaking)->LeaveLobby((GalaxyID)lobbyID, (ILobbyLeftListener* const)listener);
}

void gog_IMatchmaking_SetMaxNumLobbyMembers(gog_Interface matchmaking, gog_ID lobbyID, uint32_t maxNumLobbyMembers, gog_Interface listener){
  ((galaxy::api::IMatchmaking*)matchmaking)->SetMaxNumLobbyMembers((GalaxyID)lobbyID, (uint32_t)maxNumLobbyMembers, (ILobbyDataUpdateListener* const)listener);
}

uint32_t gog_IMatchmaking_GetMaxNumLobbyMembers(gog_Interface matchmaking, gog_ID lobbyID){
  return (uint32_t)((galaxy::api::IMatchmaking*)matchmaking)->GetMaxNumLobbyMembers((GalaxyID)lobbyID);
}

uint32_t gog_IMatchmaking_GetNumLobbyMembers(gog_Interface matchmaking, gog_ID lobbyID){
  return (uint32_t)((galaxy::api::IMatchmaking*)matchmaking)->GetNumLobbyMembers((GalaxyID)lobbyID);
}

gog_ID gog_IMatchmaking_GetLobbyMemberByIndex(gog_Interface matchmaking, gog_ID lobbyID, uint32_t index){
  return (gog_ID)((galaxy::api::IMatchmaking*)matchmaking)->GetLobbyMemberByIndex((GalaxyID)lobbyID, (uint32_t)index).ToUint64();
}

void gog_IMatchmaking_SetLobbyType(gog_Interface matchmaking, gog_ID lobbyID, enum gog_LobbyType lobbyType, gog_Interface listener){
  ((galaxy::api::IMatchmaking*)matchmaking)->SetLobbyType((GalaxyID)lobbyID, (LobbyType)lobbyType, (ILobbyDataUpdateListener* const)listener);
}

enum gog_LobbyType gog_IMatchmaking_GetLobbyType(gog_Interface matchmaking, gog_ID lobbyID){
  return (enum gog_LobbyType)((galaxy::api::IMatchmaking*)matchmaking)->GetLobbyType((GalaxyID)lobbyID);
}

void gog_IMatchmaking_SetLobbyJoinable(gog_Interface matchmaking, gog_ID lobbyID, bool joinable, gog_Interface listener){
  ((galaxy::api::IMatchmaking*)matchmaking)->SetLobbyJoinable((GalaxyID)lobbyID, (bool)joinable, (ILobbyDataUpdateListener* const)listener);
}

bool gog_IMatchmaking_IsLobbyJoinable(gog_Interface matchmaking, gog_ID lobbyID){
  return (bool)((galaxy::api::IMatchmaking*)matchmaking)->IsLobbyJoinable((GalaxyID)lobbyID);
}

void gog_IMatchmaking_RequestLobbyData(gog_Interface matchmaking, gog_ID lobbyID, gog_Interface listener){
  ((galaxy::api::IMatchmaking*)matchmaking)->RequestLobbyData((GalaxyID)lobbyID, (ILobbyDataRetrieveListener* const)listener);
}

char* gog_IMatchmaking_GetLobbyData(gog_Interface matchmaking, gog_ID lobbyID, char* key){
  return (char*)((galaxy::api::IMatchmaking*)matchmaking)->GetLobbyData((GalaxyID)lobbyID, (const char*)key);
}

void gog_IMatchmaking_GetLobbyDataCopy(gog_Interface matchmaking, gog_ID lobbyID, char* key, char* buffer, uint32_t bufferLength){
  ((galaxy::api::IMatchmaking*)matchmaking)->GetLobbyDataCopy((GalaxyID)lobbyID, (const char*)key, (char*)buffer, (uint32_t)bufferLength);
}

void gog_IMatchmaking_SetLobbyData(gog_Interface matchmaking, gog_ID lobbyID, char* key, char* value, gog_Interface listener){
  ((galaxy::api::IMatchmaking*)matchmaking)->SetLobbyData((GalaxyID)lobbyID, (const char*)key, (const char*)value, (ILobbyDataUpdateListener* const)listener);
}

uint32_t gog_IMatchmaking_GetLobbyDataCount(gog_Interface matchmaking, gog_ID lobbyID){
  return (uint32_t)((galaxy::api::IMatchmaking*)matchmaking)->GetLobbyDataCount((GalaxyID)lobbyID);
}

bool gog_IMatchmaking_GetLobbyDataByIndex(gog_Interface matchmaking, gog_ID lobbyID, uint32_t index, char* key, uint32_t keyLength, char* value, uint32_t valueLength){
  return (bool)((galaxy::api::IMatchmaking*)matchmaking)->GetLobbyDataByIndex((GalaxyID)lobbyID, (uint32_t)index, (char*)key, (uint32_t)keyLength, (char*)value, (uint32_t)valueLength);
}

void gog_IMatchmaking_DeleteLobbyData(gog_Interface matchmaking, gog_ID lobbyID, char* key, gog_Interface listener){
  ((galaxy::api::IMatchmaking*)matchmaking)->DeleteLobbyData((GalaxyID)lobbyID, (const char*)key, (ILobbyDataUpdateListener* const)listener);
}

char* gog_IMatchmaking_GetLobbyMemberData(gog_Interface matchmaking, gog_ID lobbyID, gog_ID memberID, char* key){
  return (char*)((galaxy::api::IMatchmaking*)matchmaking)->GetLobbyMemberData((GalaxyID)lobbyID, (GalaxyID)memberID, (const char*)key);
}

void gog_IMatchmaking_GetLobbyMemberDataCopy(gog_Interface matchmaking, gog_ID lobbyID, gog_ID memberID, char* key, char* buffer, uint32_t bufferLength){
  ((galaxy::api::IMatchmaking*)matchmaking)->GetLobbyMemberDataCopy((GalaxyID)lobbyID, (GalaxyID)memberID, (const char*)key, (char*)buffer, (uint32_t)bufferLength);
}

void gog_IMatchmaking_SetLobbyMemberData(gog_Interface matchmaking, gog_ID lobbyID, char* key, char* value, gog_Interface listener){
  ((galaxy::api::IMatchmaking*)matchmaking)->SetLobbyMemberData((GalaxyID)lobbyID, (const char*)key, (const char*)value, (ILobbyMemberDataUpdateListener* const)listener);
}

uint32_t gog_IMatchmaking_GetLobbyMemberDataCount(gog_Interface matchmaking, gog_ID lobbyID, gog_ID memberID){
  return (uint32_t)((galaxy::api::IMatchmaking*)matchmaking)->GetLobbyMemberDataCount((GalaxyID)lobbyID, (GalaxyID)memberID);
}

bool gog_IMatchmaking_GetLobbyMemberDataByIndex(gog_Interface matchmaking, gog_ID lobbyID, gog_ID memberID, uint32_t index, char* key, uint32_t keyLength, char* value, uint32_t valueLength){
  return (bool)((galaxy::api::IMatchmaking*)matchmaking)->GetLobbyMemberDataByIndex((GalaxyID)lobbyID, (GalaxyID)memberID, (uint32_t)index, (char*)key, (uint32_t)keyLength, (char*)value, (uint32_t)valueLength);
}

void gog_IMatchmaking_DeleteLobbyMemberData(gog_Interface matchmaking, gog_ID lobbyID, char* key, gog_Interface listener){
  ((galaxy::api::IMatchmaking*)matchmaking)->DeleteLobbyMemberData((GalaxyID)lobbyID, (const char*)key, (ILobbyMemberDataUpdateListener* const)listener);
}

gog_ID gog_IMatchmaking_GetLobbyOwner(gog_Interface matchmaking, gog_ID lobbyID){
  return (gog_ID)((galaxy::api::IMatchmaking*)matchmaking)->GetLobbyOwner((GalaxyID)lobbyID).ToUint64();
}

bool gog_IMatchmaking_SendLobbyMessage(gog_Interface matchmaking, gog_ID lobbyID, void* data, uint32_t dataSize){
  return (bool)((galaxy::api::IMatchmaking*)matchmaking)->SendLobbyMessage((GalaxyID)lobbyID, (const void*)data, (uint32_t)dataSize);
}

uint32_t gog_IMatchmaking_GetLobbyMessage(gog_Interface matchmaking, gog_ID lobbyID, uint32_t messageID, gog_ID senderID, char* msg, uint32_t msgLength){
  return (uint32_t)((galaxy::api::IMatchmaking*)matchmaking)->GetLobbyMessage((GalaxyID)lobbyID, (uint32_t)messageID, (GalaxyID&)senderID, (char*)msg, (uint32_t)msgLength);
}

bool gog_INetworking_SendP2PPacket(gog_Interface networking, gog_ID galaxyID, const void* data, uint32_t dataSize, enum gog_P2PSendType sendType, uint8_t channel){
  return (bool)((galaxy::api::INetworking*)networking)->SendP2PPacket((GalaxyID)galaxyID, (const void*)data, (uint32_t)dataSize, (P2PSendType)sendType, (uint8_t)channel);
}

bool gog_INetworking_PeekP2PPacket(gog_Interface networking, void* dest, uint32_t destSize, uint32_t* outMsgSize, gog_ID outGalaxyID, uint8_t channel){
  return (bool)((galaxy::api::INetworking*)networking)->PeekP2PPacket((void*)dest, (uint32_t)destSize, (uint32_t*)outMsgSize, (GalaxyID&)outGalaxyID, (uint8_t)channel);
}

bool gog_INetworking_IsP2PPacketAvailable(gog_Interface networking, uint32_t* outMsgSize, uint8_t channel){
  return (bool)((galaxy::api::INetworking*)networking)->IsP2PPacketAvailable((uint32_t*)outMsgSize, (uint8_t)channel);
}

bool gog_INetworking_ReadP2PPacket(gog_Interface networking, void* dest, uint32_t destSize, uint32_t* outMsgSize, gog_ID outGalaxyID, uint8_t channel){
  return (bool)((galaxy::api::INetworking*)networking)->ReadP2PPacket((void*)dest, (uint32_t)destSize, (uint32_t*)outMsgSize, (GalaxyID&)outGalaxyID, (uint8_t)channel);
}

void gog_INetworking_PopP2PPacket(gog_Interface networking, uint8_t channel){
  ((galaxy::api::INetworking*)networking)->PopP2PPacket((uint8_t)channel);
}

int gog_INetworking_GetPingWith(gog_Interface networking, gog_ID galaxyID){
  return (int)((galaxy::api::INetworking*)networking)->GetPingWith((GalaxyID)galaxyID);
}

void gog_INetworking_RequestNatTypeDetection(gog_Interface networking){
  ((galaxy::api::INetworking*)networking)->RequestNatTypeDetection();
}

enum gog_NatType gog_INetworking_GetNatType(gog_Interface networking){
  return (enum gog_NatType)((galaxy::api::INetworking*)networking)->GetNatType();
}

enum gog_ConnectionType gog_INetworking_GetConnectionType(gog_Interface networking, gog_ID userID){
  return (enum gog_ConnectionType)((galaxy::api::INetworking*)networking)->GetConnectionType((GalaxyID)userID);
}

void gog_IStats_RequestUserStatsAndAchievements(gog_Interface stats, gog_ID userID, gog_Interface listener){
  ((galaxy::api::IStats*)stats)->RequestUserStatsAndAchievements((GalaxyID)userID, (IUserStatsAndAchievementsRetrieveListener* const)listener);
}

int32_t gog_IStats_GetStatInt(gog_Interface stats, char* name, gog_ID userID){
  return (int32_t)((galaxy::api::IStats*)stats)->GetStatInt((const char*)name, (GalaxyID)userID);
}

float gog_IStats_GetStatFloat(gog_Interface stats, char* name, gog_ID userID){
  return (float)((galaxy::api::IStats*)stats)->GetStatFloat((const char*)name, (GalaxyID)userID);
}

void gog_IStats_SetStatInt(gog_Interface stats, char* name, int32_t value){
  ((galaxy::api::IStats*)stats)->SetStatInt((const char*)name, (int32_t)value);
}

void gog_IStats_SetStatFloat(gog_Interface stats, char* name, float value){
  ((galaxy::api::IStats*)stats)->SetStatFloat((const char*)name, (float)value);
}

void gog_IStats_UpdateAvgRateStat(gog_Interface stats, char* name, float countThisSession, double sessionLength){
  ((galaxy::api::IStats*)stats)->UpdateAvgRateStat((const char*)name, (float)countThisSession, (double)sessionLength);
}

void gog_IStats_GetAchievement(gog_Interface stats, char* name, bool* unlocked, uint32_t* unlockTime, gog_ID userID){
  ((galaxy::api::IStats*)stats)->GetAchievement((const char*)name, (bool&)unlocked, (uint32_t&)unlockTime, (GalaxyID)userID);
}

void gog_IStats_SetAchievement(gog_Interface stats, char* name){
  ((galaxy::api::IStats*)stats)->SetAchievement((const char*)name);
}

void gog_IStats_ClearAchievement(gog_Interface stats, char* name){
  ((galaxy::api::IStats*)stats)->ClearAchievement((const char*)name);
}

void gog_IStats_StoreStatsAndAchievements(gog_Interface stats, gog_Interface listener){
  ((galaxy::api::IStats*)stats)->StoreStatsAndAchievements((IStatsAndAchievementsStoreListener* const)listener);
}

void gog_IStats_ResetStatsAndAchievements(gog_Interface stats, gog_Interface listener){
  ((galaxy::api::IStats*)stats)->ResetStatsAndAchievements((IStatsAndAchievementsStoreListener* const)listener);
}

char* gog_IStats_GetAchievementDisplayName(gog_Interface stats, char* name){
  return (char*)((galaxy::api::IStats*)stats)->GetAchievementDisplayName((const char*)name);
}

void gog_IStats_GetAchievementDisplayNameCopy(gog_Interface stats, char* name, char* buffer, uint32_t bufferLength){
  ((galaxy::api::IStats*)stats)->GetAchievementDisplayNameCopy((const char*)name, (char*)buffer, (uint32_t)bufferLength);
}

char* gog_IStats_GetAchievementDescription(gog_Interface stats, char* name){
  return (char*)((galaxy::api::IStats*)stats)->GetAchievementDescription((const char*)name);
}

void gog_IStats_GetAchievementDescriptionCopy(gog_Interface stats, char* name, char* buffer, uint32_t bufferLength){
  ((galaxy::api::IStats*)stats)->GetAchievementDescriptionCopy((const char*)name, (char*)buffer, (uint32_t)bufferLength);
}

bool gog_IStats_IsAchievementVisible(gog_Interface stats, char* name){
  return (bool)((galaxy::api::IStats*)stats)->IsAchievementVisible((const char*)name);
}

bool gog_IStats_IsAchievementVisibleWhileLocked(gog_Interface stats, char* name){
  return (bool)((galaxy::api::IStats*)stats)->IsAchievementVisibleWhileLocked((const char*)name);
}

void gog_IStats_RequestLeaderboards(gog_Interface stats, gog_Interface listener){
  ((galaxy::api::IStats*)stats)->RequestLeaderboards((ILeaderboardsRetrieveListener* const)listener);
}

char* gog_IStats_GetLeaderboardDisplayName(gog_Interface stats, char* name){
  return (char*)((galaxy::api::IStats*)stats)->GetLeaderboardDisplayName((const char*)name);
}

void gog_IStats_GetLeaderboardDisplayNameCopy(gog_Interface stats, char* name, char* buffer, uint32_t bufferLength){
  ((galaxy::api::IStats*)stats)->GetLeaderboardDisplayNameCopy((const char*)name, (char*)buffer, (uint32_t)bufferLength);
}

enum gog_LeaderboardSortMethod gog_IStats_GetLeaderboardSortMethod(gog_Interface stats, char* name){
  return (enum gog_LeaderboardSortMethod)((galaxy::api::IStats*)stats)->GetLeaderboardSortMethod((const char*)name);
}

enum gog_LeaderboardDisplayType gog_IStats_GetLeaderboardDisplayType(gog_Interface stats, char* name){
  return (enum gog_LeaderboardDisplayType)((galaxy::api::IStats*)stats)->GetLeaderboardDisplayType((const char*)name);
}

void gog_IStats_RequestLeaderboardEntriesGlobal(gog_Interface stats, char* name, uint32_t rangeStart, uint32_t rangeEnd, gog_Interface listener){
  ((galaxy::api::IStats*)stats)->RequestLeaderboardEntriesGlobal((const char*)name, (uint32_t)rangeStart, (uint32_t)rangeEnd, (ILeaderboardEntriesRetrieveListener* const)listener);
}

void gog_IStats_RequestLeaderboardEntriesAroundUser(gog_Interface stats, char* name, uint32_t countBefore, uint32_t countAfter, gog_ID userID, gog_Interface listener){
  ((galaxy::api::IStats*)stats)->RequestLeaderboardEntriesAroundUser((const char*)name, (uint32_t)countBefore, (uint32_t)countAfter, (GalaxyID)userID, (ILeaderboardEntriesRetrieveListener* const)listener);
}

void gog_IStats_RequestLeaderboardEntriesForUsers(gog_Interface stats, char* name, gog_ID* userArray, uint32_t userArraySize, gog_Interface listener){
  ((galaxy::api::IStats*)stats)->RequestLeaderboardEntriesForUsers((const char*)name, (GalaxyID*)userArray, (uint32_t)userArraySize, (ILeaderboardEntriesRetrieveListener* const)listener);
}

void gog_IStats_GetRequestedLeaderboardEntry(gog_Interface stats, uint32_t index, uint32_t* rank, int32_t* score, gog_ID* userID){
  ((galaxy::api::IStats*)stats)->GetRequestedLeaderboardEntry((uint32_t)index, (uint32_t&)rank, (int32_t&)score, (GalaxyID&)userID);
}

void gog_IStats_GetRequestedLeaderboardEntryWithDetails(gog_Interface stats, uint32_t index, uint32_t* rank, int32_t* score, void* details, uint32_t detailsSize, uint32_t* outDetailsSize, gog_ID* userID){
  ((galaxy::api::IStats*)stats)->GetRequestedLeaderboardEntryWithDetails((uint32_t)index, (uint32_t&)rank, (int32_t&)score, (void*)details, (uint32_t)detailsSize, (uint32_t&)outDetailsSize, (GalaxyID&)userID);
}

void gog_IStats_SetLeaderboardScore(gog_Interface stats, char* name, int32_t score, bool forceUpdate, gog_Interface listener){
  ((galaxy::api::IStats*)stats)->SetLeaderboardScore((const char*)name, (int32_t)score, (bool)forceUpdate, (ILeaderboardScoreUpdateListener* const)listener);
}

void gog_IStats_SetLeaderboardScoreWithDetails(gog_Interface stats, char* name, int32_t score, void* details, uint32_t detailsSize, bool forceUpdate, gog_Interface listener){
  ((galaxy::api::IStats*)stats)->SetLeaderboardScoreWithDetails((const char*)name, (int32_t)score, (const void*)details, (uint32_t)detailsSize, (bool)forceUpdate, (ILeaderboardScoreUpdateListener* const)listener);
}

uint32_t gog_IStats_GetLeaderboardEntryCount(gog_Interface stats, char* name){
  return (uint32_t)((galaxy::api::IStats*)stats)->GetLeaderboardEntryCount((const char*)name);
}

void gog_IStats_FindLeaderboard(gog_Interface stats, char* name, gog_Interface listener){
  ((galaxy::api::IStats*)stats)->FindLeaderboard((const char*)name, (ILeaderboardRetrieveListener* const)listener);
}

void gog_IStats_FindOrCreateLeaderboard(gog_Interface stats, char* name, char* displayName, enum gog_LeaderboardSortMethod sortMethod, enum gog_LeaderboardDisplayType displayType, gog_Interface listener){
  ((galaxy::api::IStats*)stats)->FindOrCreateLeaderboard((const char*)name, (const char*)displayName, (const LeaderboardSortMethod&)sortMethod, (const LeaderboardDisplayType&)displayType, (ILeaderboardRetrieveListener* const)listener);
}

void gog_IStats_RequestUserTimePlayed(gog_Interface stats, gog_ID userID, gog_Interface listener){
  ((galaxy::api::IStats*)stats)->RequestUserTimePlayed((GalaxyID)userID, (IUserTimePlayedRetrieveListener* const)listener);
}

uint32_t gog_IStats_GetUserTimePlayed(gog_Interface stats, gog_ID userID){
  return (uint32_t)((galaxy::api::IStats*)stats)->GetUserTimePlayed((GalaxyID)userID);
}

void gog_IUtils_GetImageSize(gog_Interface utils, uint32_t imageID, int32_t *width, int32_t *height){
  ((galaxy::api::IUtils*)utils)->GetImageSize((uint32_t)imageID, (int32_t&)*width, (int32_t&)*height);
}

void gog_IUtils_GetImageRGBA(gog_Interface utils, uint32_t imageID, void* buffer, uint32_t bufferLength){
  ((galaxy::api::IUtils*)utils)->GetImageRGBA((uint32_t)imageID, (void*)buffer, (uint32_t)bufferLength);
}

void gog_IUtils_RegisterForNotification(gog_Interface utils, char* type){
  ((galaxy::api::IUtils*)utils)->RegisterForNotification((const char*)type);
}

uint32_t gog_IUtils_GetNotification(gog_Interface utils, gog_NotificationID notificationID, bool *consumable, char* type, uint32_t typeLength, void* content, uint32_t contentSize){
  return (uint32_t)((galaxy::api::IUtils*)utils)->GetNotification((NotificationID)notificationID, (bool&)*consumable, (char*)type, (uint32_t)typeLength, (void*)content, (uint32_t)contentSize);
}

void gog_IUtils_ShowOverlayWithWebPage(gog_Interface utils, char* url){
  ((galaxy::api::IUtils*)utils)->ShowOverlayWithWebPage((const char*)url);
}

bool gog_IUtils_IsOverlayVisible(gog_Interface utils){
  return (bool)((galaxy::api::IUtils*)utils)->IsOverlayVisible();
}

enum gog_OverlayState gog_IUtils_GetOverlayState(gog_Interface utils){
  return (enum gog_OverlayState)((galaxy::api::IUtils*)utils)->GetOverlayState();
}

void gog_IUtils_DisableOverlayPopups(gog_Interface utils, char* popupGroup){
  ((galaxy::api::IUtils*)utils)->DisableOverlayPopups((const char*)popupGroup);
}

enum gog_ServicesConnectionState gog_IUtils_GetGogServicesConnectionState(gog_Interface utils){
  return (enum gog_ServicesConnectionState)((galaxy::api::IUtils*)utils)->GetGogServicesConnectionState();
}

bool gog_IApps_IsDlcInstalled(gog_Interface apps, gog_ProductID productID){
  return (bool)((galaxy::api::IApps*)apps)->IsDlcInstalled((ProductID)productID);
}

void gog_IApps_IsDlcOwned(gog_Interface apps, gog_ProductID productID, gog_Interface listener){
  ((galaxy::api::IApps*)apps)->IsDlcOwned((ProductID)productID, (IIsDlcOwnedListener* const)listener);
}

char* gog_IApps_GetCurrentGameLanguage(gog_Interface apps, gog_ProductID productID){
  return (char*)((galaxy::api::IApps*)apps)->GetCurrentGameLanguage((ProductID)productID);
}

void gog_IApps_GetCurrentGameLanguageCopy(gog_Interface apps, char* buffer, uint32_t bufferLength, gog_ProductID productID){
  ((galaxy::api::IApps*)apps)->GetCurrentGameLanguageCopy((char*)buffer, (uint32_t)bufferLength, (ProductID)productID);
}

char* gog_IApps_GetCurrentGameLanguageCode(gog_Interface apps, gog_ProductID productID){
  return (char*)((galaxy::api::IApps*)apps)->GetCurrentGameLanguageCode((ProductID)productID);
}

void gog_IApps_GetCurrentGameLanguageCodeCopy(gog_Interface apps, char* buffer, uint32_t bufferLength, gog_ProductID productID){
  ((galaxy::api::IApps*)apps)->GetCurrentGameLanguageCodeCopy((char*)buffer, (uint32_t)bufferLength, (ProductID)productID);
}

void gog_IStorage_FileWrite(gog_Interface storage, const char* fileName, const void* data, uint32_t dataSize){
  ((galaxy::api::IStorage*)storage)->FileWrite((const char*)fileName, (const void*)data, (uint32_t)dataSize);
}

uint32_t gog_IStorage_FileRead(gog_Interface storage, const char* fileName, void* data, uint32_t dataSize){
  return (uint32_t)((galaxy::api::IStorage*)storage)->FileRead((const char*)fileName, (void*)data, (uint32_t)dataSize);
}

void gog_IStorage_FileDelete(gog_Interface storage, const char* fileName){
  ((galaxy::api::IStorage*)storage)->FileDelete((const char*)fileName);
}

bool gog_IStorage_FileExists(gog_Interface storage, const char* fileName){
  return (bool)((galaxy::api::IStorage*)storage)->FileExists((const char*)fileName);
}

uint32_t gog_IStorage_GetFileSize(gog_Interface storage, const char* fileName){
  return (uint32_t)((galaxy::api::IStorage*)storage)->GetFileSize((const char*)fileName);
}

uint32_t gog_IStorage_GetFileTimestamp(gog_Interface storage, const char* fileName){
  return (uint32_t)((galaxy::api::IStorage*)storage)->GetFileTimestamp((const char*)fileName);
}

uint32_t gog_IStorage_GetFileCount(gog_Interface storage){
  return (uint32_t)((galaxy::api::IStorage*)storage)->GetFileCount();
}

char* gog_IStorage_GetFileNameByIndex(gog_Interface storage, uint32_t index){
  return (char*)((galaxy::api::IStorage*)storage)->GetFileNameByIndex((uint32_t)index);
}

void gog_IStorage_GetFileNameCopyByIndex(gog_Interface storage, uint32_t index, char* buffer, uint32_t bufferLength){
  ((galaxy::api::IStorage*)storage)->GetFileNameCopyByIndex((uint32_t)index, (char*)buffer, (uint32_t)bufferLength);
}

void gog_IStorage_FileShare(gog_Interface storage, const char* fileName, gog_Interface listener){
  ((galaxy::api::IStorage*)storage)->FileShare((const char*)fileName, (IFileShareListener* const)listener);
}

void gog_IStorage_DownloadSharedFile(gog_Interface storage, gog_SharedFileID sharedFileID, gog_Interface listener){
  ((galaxy::api::IStorage*)storage)->DownloadSharedFile((SharedFileID)sharedFileID, (ISharedFileDownloadListener* const)listener);
}

char* gog_IStorage_GetSharedFileName(gog_Interface storage, gog_SharedFileID sharedFileID){
  return (char*)((galaxy::api::IStorage*)storage)->GetSharedFileName((SharedFileID)sharedFileID);
}

void gog_IStorage_GetSharedFileNameCopy(gog_Interface storage, gog_SharedFileID sharedFileID, char* buffer, uint32_t bufferLength){
  ((galaxy::api::IStorage*)storage)->GetSharedFileNameCopy((SharedFileID)sharedFileID, (char*)buffer, (uint32_t)bufferLength);
}

uint32_t gog_IStorage_GetSharedFileSize(gog_Interface storage, gog_SharedFileID sharedFileID){
  return (uint32_t)((galaxy::api::IStorage*)storage)->GetSharedFileSize((SharedFileID)sharedFileID);
}

gog_ID gog_IStorage_GetSharedFileOwner(gog_Interface storage, gog_SharedFileID sharedFileID){
  return (gog_ID)((galaxy::api::IStorage*)storage)->GetSharedFileOwner((SharedFileID)sharedFileID).ToUint64();
}

uint32_t gog_IStorage_SharedFileRead(gog_Interface storage, gog_SharedFileID sharedFileID, void* data, uint32_t dataSize, uint32_t offset){
  return (uint32_t)((galaxy::api::IStorage*)storage)->SharedFileRead((SharedFileID)sharedFileID, (void*)data, (uint32_t)dataSize, (uint32_t)offset);
}

void gog_IStorage_SharedFileClose(gog_Interface storage, gog_SharedFileID sharedFileID){
  ((galaxy::api::IStorage*)storage)->SharedFileClose((SharedFileID)sharedFileID);
}

uint32_t gog_IStorage_GetDownloadedSharedFileCount(gog_Interface storage){
  return (uint32_t)((galaxy::api::IStorage*)storage)->GetDownloadedSharedFileCount();
}

gog_SharedFileID gog_IStorage_GetDownloadedSharedFileByIndex(gog_Interface storage, uint32_t index){
  return (gog_SharedFileID)((galaxy::api::IStorage*)storage)->GetDownloadedSharedFileByIndex((uint32_t)index);
}

void gog_ICustomNetworking_OpenConnection(gog_Interface customnetworking, const char* connectionString, gog_Interface listener){
  ((galaxy::api::ICustomNetworking*)customnetworking)->OpenConnection((const char*)connectionString, (IConnectionOpenListener* const)listener);
}

void gog_ICustomNetworking_CloseConnection(gog_Interface customnetworking, gog_ConnectionID connectionID, gog_Interface listener){
  ((galaxy::api::ICustomNetworking*)customnetworking)->CloseConnection((ConnectionID)connectionID, (IConnectionCloseListener* const)listener);
}

void gog_ICustomNetworking_SendData(gog_Interface customnetworking, gog_ConnectionID connectionID, const void* data, uint32_t dataSize){
  ((galaxy::api::ICustomNetworking*)customnetworking)->SendData((ConnectionID)connectionID, (const void*)data, (uint32_t)dataSize);
}

uint32_t gog_ICustomNetworking_GetAvailableDataSize(gog_Interface customnetworking, gog_ConnectionID connectionID){
  return (uint32_t)((galaxy::api::ICustomNetworking*)customnetworking)->GetAvailableDataSize((ConnectionID)connectionID);
}

void gog_ICustomNetworking_PeekData(gog_Interface customnetworking, gog_ConnectionID connectionID, void* dest, uint32_t dataSize){
  ((galaxy::api::ICustomNetworking*)customnetworking)->PeekData((ConnectionID)connectionID, (void*)dest, (uint32_t)dataSize);
}

void gog_ICustomNetworking_ReadData(gog_Interface customnetworking, gog_ConnectionID connectionID, void* dest, uint32_t dataSize){
  ((galaxy::api::ICustomNetworking*)customnetworking)->ReadData((ConnectionID)connectionID, (void*)dest, (uint32_t)dataSize);
}

void gog_ICustomNetworking_PopData(gog_Interface customnetworking, gog_ConnectionID connectionID, uint32_t dataSize){
  ((galaxy::api::ICustomNetworking*)customnetworking)->PopData((ConnectionID)connectionID, (uint32_t)dataSize);
}

void gog_ITelemetry_AddStringParam(gog_Interface telemetry, const char* name, const char* value){
  ((galaxy::api::ITelemetry*)telemetry)->AddStringParam((const char*)name, (const char*)value);
}

void gog_ITelemetry_AddIntParam(gog_Interface telemetry, const char* name, int32_t value){
  ((galaxy::api::ITelemetry*)telemetry)->AddIntParam((const char*)name, (int32_t)value);
}

void gog_ITelemetry_AddFloatParam(gog_Interface telemetry, const char* name, double value){
  ((galaxy::api::ITelemetry*)telemetry)->AddFloatParam((const char*)name, (double)value);
}

void gog_ITelemetry_AddBoolParam(gog_Interface telemetry, const char* name, bool value){
  ((galaxy::api::ITelemetry*)telemetry)->AddBoolParam((const char*)name, (bool)value);
}

void gog_ITelemetry_AddObjectParam(gog_Interface telemetry, const char* name){
  ((galaxy::api::ITelemetry*)telemetry)->AddObjectParam((const char*)name);
}

void gog_ITelemetry_AddArrayParam(gog_Interface telemetry, const char* name){
  ((galaxy::api::ITelemetry*)telemetry)->AddArrayParam((const char*)name);
}

void gog_ITelemetry_CloseParam(gog_Interface telemetry){
  ((galaxy::api::ITelemetry*)telemetry)->CloseParam();
}

void gog_ITelemetry_ClearParams(gog_Interface telemetry){
  ((galaxy::api::ITelemetry*)telemetry)->ClearParams();
}

void gog_ITelemetry_SetSamplingClass(gog_Interface telemetry, const char* name){
  ((galaxy::api::ITelemetry*)telemetry)->SetSamplingClass((const char*)name);
}

uint32_t gog_ITelemetry_SendTelemetryEvent(gog_Interface telemetry, const char* eventType, gog_Interface listener){
  return (uint32_t)((galaxy::api::ITelemetry*)telemetry)->SendTelemetryEvent((const char*)eventType, (ITelemetryEventSendListener* const)listener);
}

uint32_t gog_ITelemetry_SendAnonymousTelemetryEvent(gog_Interface telemetry, const char* eventType, gog_Interface listener){
  return (uint32_t)((galaxy::api::ITelemetry*)telemetry)->SendAnonymousTelemetryEvent((const char*)eventType, (ITelemetryEventSendListener* const)listener);
}

char* gog_ITelemetry_GetVisitID(gog_Interface telemetry){
  return (char*)((galaxy::api::ITelemetry*)telemetry)->GetVisitID();
}

void gog_ITelemetry_GetVisitIDCopy(gog_Interface telemetry, char* buffer, uint32_t bufferLength){
  ((galaxy::api::ITelemetry*)telemetry)->GetVisitIDCopy((char*)buffer, (uint32_t)bufferLength);
}

void gog_ITelemetry_ResetVisitID(gog_Interface telemetry){
  ((galaxy::api::ITelemetry*)telemetry)->ResetVisitID();
}

void gog_ICloudStorage_GetFileList(gog_Interface cloudstorage, const char* container, gog_Interface listener){
  ((galaxy::api::ICloudStorage*)cloudstorage)->GetFileList((const char*)container, (ICloudStorageGetFileListListener*)listener);
}

char* gog_ICloudStorage_GetFileNameByIndex(gog_Interface cloudstorage, uint32_t index){
  return (char*)((galaxy::api::ICloudStorage*)cloudstorage)->GetFileNameByIndex((uint32_t)index);
}

uint32_t gog_ICloudStorage_GetFileSizeByIndex(gog_Interface cloudstorage, uint32_t index){
  return (uint32_t)((galaxy::api::ICloudStorage*)cloudstorage)->GetFileSizeByIndex((uint32_t)index);
}

uint32_t gog_ICloudStorage_GetFileTimestampByIndex(gog_Interface cloudstorage, uint32_t index){
  return (uint32_t)((galaxy::api::ICloudStorage*)cloudstorage)->GetFileTimestampByIndex((uint32_t)index);
}

char* gog_ICloudStorage_GetFileHashByIndex(gog_Interface cloudstorage, uint32_t index){
  return (char*)((galaxy::api::ICloudStorage*)cloudstorage)->GetFileHashByIndex((uint32_t)index);
}

void gog_ICloudStorage_GetFileMetadata(gog_Interface cloudstorage, const char* container, const char* name, gog_Interface listener){
  ((galaxy::api::ICloudStorage*)cloudstorage)->GetFileMetadata((const char*)container, (const char*)name, (ICloudStorageGetFileListener*)listener);
}

void gog_ICloudStorage_DeleteFile(gog_Interface cloudstorage, const char* container, const char* name, gog_Interface listener, const char* expectedHash){
  ((galaxy::api::ICloudStorage*)cloudstorage)->DeleteFile((const char*)container, (const char*)name, (ICloudStorageDeleteFileListener*)listener, (const char*)expectedHash);
}

void gog_ICloudStorage_OpenSavegame(gog_Interface cloudstorage){
  ((galaxy::api::ICloudStorage*)cloudstorage)->OpenSavegame();
}

void gog_ICloudStorage_CloseSavegame(gog_Interface cloudstorage){
  ((galaxy::api::ICloudStorage*)cloudstorage)->CloseSavegame();
}

