#include <galaxy/GalaxyApi.h>
#include <cstring>
#include "gog.h"

using namespace galaxy::api;

class Listener : 
    public IIsDlcOwnedListener,
    public IChatRoomWithUserRetrieveListener,
    public IChatRoomMessageSendListener,
    public IChatRoomMessagesListener,
    public IChatRoomMessagesRetrieveListener,
    public ICloudStorageGetFileListListener,
    public ICloudStorageGetFileListener,
    public ICloudStoragePutFileListener,
    public ICloudStorageDeleteFileListener,
    public IConnectionOpenListener,
    public IConnectionCloseListener,
    public IConnectionDataListener,
    public IPersonaDataChangedListener,
    public IUserInformationRetrieveListener,
    public IFriendListListener,
    public IFriendInvitationSendListener,
    public IFriendInvitationListRetrieveListener,
    public ISentFriendInvitationListRetrieveListener,
    public IFriendInvitationListener,
    public IFriendInvitationRespondToListener,
    public IFriendAddListener,
    public IFriendDeleteListener,
    public IRichPresenceChangeListener,
    public IRichPresenceListener,
    public IRichPresenceRetrieveListener,
    public IGameJoinRequestedListener,
    public IGameInvitationReceivedListener,
    public ISendInvitationListener,
    public IUserFindListener,
    public ILobbyListListener,
    public ILobbyCreatedListener,
    public ILobbyEnteredListener,
    public ILobbyLeftListener,
    public ILobbyDataListener,
    public ILobbyDataUpdateListener,
    public ILobbyMemberDataUpdateListener,
    public ILobbyDataRetrieveListener,
    public ILobbyMemberStateListener,
    public ILobbyOwnerChangeListener,
    public ILobbyMessageListener,
    public INetworkingListener,
    public INatTypeDetectionListener,
    public IUserStatsAndAchievementsRetrieveListener,
    public IStatsAndAchievementsStoreListener,
    public IAchievementChangeListener,
    public ILeaderboardsRetrieveListener,
    public ILeaderboardEntriesRetrieveListener,
    public ILeaderboardScoreUpdateListener,
    public ILeaderboardRetrieveListener,
    public IUserTimePlayedRetrieveListener,
    public IFileShareListener,
    public ISharedFileDownloadListener,
    public ITelemetryEventSendListener,
    public IAuthListener,
    public IOtherSessionStartListener,
    public IOperationalStateChangeListener,
    public IUserDataListener,
    public ISpecificUserDataListener,
    public IEncryptedAppTicketListener,
    public IAccessTokenListener,
    public IPlayFabCreateOpenIDConnectionListener,
    public IPlayFabLoginWithOpenIDConnectListener,
    public IOverlayVisibilityChangeListener,
    public IOverlayInitializationStateChangeListener,
    public INotificationListener,
    public IGogServicesConnectionStateListener {
public:
  struct gog_listener listener;

void OnAuthSuccess() override {
  if(!this->listener.OnAuthSuccess) return;
  this->listener.OnAuthSuccess(this->listener.userptr);
}

void OnAuthFailure(galaxy::api::IAuthListener::FailureReason failureReason) override {
  if(!this->listener.OnAuthFailure) return;
  this->listener.OnAuthFailure(this->listener.userptr, (enum gog_AuthFailureReason)failureReason);
}

void OnAuthLost() override {
  if(!this->listener.OnAuthLost) return;
  this->listener.OnAuthLost(this->listener.userptr);
}

void OnOtherSessionStarted() override {
  if(!this->listener.OnOtherSessionStarted) return;
  this->listener.OnOtherSessionStarted(this->listener.userptr);
}

void OnOperationalStateChanged(uint32_t operationalState) override {
  if(!this->listener.OnOperationalStateChanged) return;
  this->listener.OnOperationalStateChanged(this->listener.userptr, (enum gog_OperationalState)operationalState);
}

void OnUserDataUpdated() override {
  if(!this->listener.OnUserDataUpdated) return;
  this->listener.OnUserDataUpdated(this->listener.userptr);
}

void OnSpecificUserDataUpdated(GalaxyID userID) override {
  if(!this->listener.OnSpecificUserDataUpdated) return;
  this->listener.OnSpecificUserDataUpdated(this->listener.userptr, (gog_ID)userID.ToUint64());
}

void OnEncryptedAppTicketRetrieveSuccess() override {
  if(!this->listener.OnEncryptedAppTicketRetrieveSuccess) return;
  this->listener.OnEncryptedAppTicketRetrieveSuccess(this->listener.userptr);
}

void OnEncryptedAppTicketRetrieveFailure(galaxy::api::IEncryptedAppTicketListener::FailureReason failureReason) override {
  if(!this->listener.OnEncryptedAppTicketRetrieveFailure) return;
  this->listener.OnEncryptedAppTicketRetrieveFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
}

void OnAccessTokenChanged() override {
  if(!this->listener.OnAccessTokenChanged) return;
  this->listener.OnAccessTokenChanged(this->listener.userptr);
}

void OnPlayFabCreateOpenIDConnectionSuccess(bool connectionAlreadyExists) override {
  if(!this->listener.OnPlayFabCreateOpenIDConnectionSuccess) return;
  this->listener.OnPlayFabCreateOpenIDConnectionSuccess(this->listener.userptr, (bool)connectionAlreadyExists);
}

void OnPlayFabCreateOpenIDConnectionFailure(galaxy::api::IPlayFabCreateOpenIDConnectionListener::FailureReason failureReason) override {
  if(!this->listener.OnPlayFabCreateOpenIDConnectionFailure) return;
  this->listener.OnPlayFabCreateOpenIDConnectionFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
}

void OnPlayFabLoginWithOpenIDConnectSuccess() override {
  if(!this->listener.OnPlayFabLoginWithOpenIDConnectSuccess) return;
  this->listener.OnPlayFabLoginWithOpenIDConnectSuccess(this->listener.userptr);
}

void OnPlayFabLoginWithOpenIDConnectFailure(galaxy::api::IPlayFabLoginWithOpenIDConnectListener::FailureReason failureReason) override {
  if(!this->listener.OnPlayFabLoginWithOpenIDConnectFailure) return;
  this->listener.OnPlayFabLoginWithOpenIDConnectFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
}

void OnPersonaDataChanged(GalaxyID userID, uint32_t personaStateChange) override {
  if(!this->listener.OnPersonaDataChanged) return;
  this->listener.OnPersonaDataChanged(this->listener.userptr, (gog_ID)userID.ToUint64(), (uint32_t)personaStateChange);
}

void OnUserInformationRetrieveSuccess(GalaxyID userID) override {
  if(!this->listener.OnUserInformationRetrieveSuccess) return;
  this->listener.OnUserInformationRetrieveSuccess(this->listener.userptr, (gog_ID)userID.ToUint64());
}

void OnUserInformationRetrieveFailure(GalaxyID userID, galaxy::api::IUserInformationRetrieveListener::FailureReason failureReason) override {
  if(!this->listener.OnUserInformationRetrieveFailure) return;
  this->listener.OnUserInformationRetrieveFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FailureReason)failureReason);
}

void OnFriendListRetrieveSuccess() override {
  if(!this->listener.OnFriendListRetrieveSuccess) return;
  this->listener.OnFriendListRetrieveSuccess(this->listener.userptr);
}

void OnFriendListRetrieveFailure(galaxy::api::IFriendListListener::FailureReason failureReason) override {
  if(!this->listener.OnFriendListRetrieveFailure) return;
  this->listener.OnFriendListRetrieveFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
}

void OnFriendInvitationSendSuccess(GalaxyID userID) override {
  if(!this->listener.OnFriendInvitationSendSuccess) return;
  this->listener.OnFriendInvitationSendSuccess(this->listener.userptr, (gog_ID)userID.ToUint64());
}

void OnFriendInvitationSendFailure(GalaxyID userID, galaxy::api::IFriendInvitationSendListener::FailureReason failureReason) override {
  if(!this->listener.OnFriendInvitationSendFailure) return;
  this->listener.OnFriendInvitationSendFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FriendInvitationFailureReason)failureReason);
}

void OnFriendInvitationListRetrieveSuccess() override {
  if(!this->listener.OnFriendInvitationListRetrieveSuccess) return;
  this->listener.OnFriendInvitationListRetrieveSuccess(this->listener.userptr);
}

void OnFriendInvitationListRetrieveFailure(galaxy::api::IFriendInvitationListRetrieveListener::FailureReason failureReason) override {
  if(!this->listener.OnFriendInvitationListRetrieveFailure) return;
  this->listener.OnFriendInvitationListRetrieveFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
}

void OnSentFriendInvitationListRetrieveSuccess() override {
  if(!this->listener.OnSentFriendInvitationListRetrieveSuccess) return;
  this->listener.OnSentFriendInvitationListRetrieveSuccess(this->listener.userptr);
}

void OnSentFriendInvitationListRetrieveFailure(galaxy::api::ISentFriendInvitationListRetrieveListener::FailureReason failureReason) override {
  if(!this->listener.OnSentFriendInvitationListRetrieveFailure) return;
  this->listener.OnSentFriendInvitationListRetrieveFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
}

void OnFriendInvitationReceived(GalaxyID userID, uint32_t sendTime) override {
  if(!this->listener.OnFriendInvitationReceived) return;
  this->listener.OnFriendInvitationReceived(this->listener.userptr, (gog_ID)userID.ToUint64(), (uint32_t)sendTime);
}

void OnFriendInvitationRespondToSuccess(GalaxyID userID, bool accept) override {
  if(!this->listener.OnFriendInvitationRespondToSuccess) return;
  this->listener.OnFriendInvitationRespondToSuccess(this->listener.userptr, (gog_ID)userID.ToUint64(), (bool)accept);
}

void OnFriendInvitationRespondToFailure(GalaxyID userID, galaxy::api::IFriendInvitationRespondToListener::FailureReason failureReason) override {
  if(!this->listener.OnFriendInvitationRespondToFailure) return;
  this->listener.OnFriendInvitationRespondToFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FriendInvitationRespondFailureReason)failureReason);
}

void OnFriendAdded(GalaxyID userID, InvitationDirection invitationDirection) override {
  if(!this->listener.OnFriendAdded) return;
  this->listener.OnFriendAdded(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_InvitationDirection)invitationDirection);
}

void OnFriendDeleteSuccess(GalaxyID userID) override {
  if(!this->listener.OnFriendDeleteSuccess) return;
  this->listener.OnFriendDeleteSuccess(this->listener.userptr, (gog_ID)userID.ToUint64());
}

void OnFriendDeleteFailure(GalaxyID userID, galaxy::api::IFriendDeleteListener::FailureReason failureReason) override {
  if(!this->listener.OnFriendDeleteFailure) return;
  this->listener.OnFriendDeleteFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FailureReason)failureReason);
}

void OnRichPresenceChangeSuccess() override {
  if(!this->listener.OnRichPresenceChangeSuccess) return;
  this->listener.OnRichPresenceChangeSuccess(this->listener.userptr);
}

void OnRichPresenceChangeFailure(galaxy::api::IRichPresenceChangeListener::FailureReason failureReason) override {
  if(!this->listener.OnRichPresenceChangeFailure) return;
  this->listener.OnRichPresenceChangeFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
}

void OnRichPresenceUpdated(GalaxyID userID) override {
  if(!this->listener.OnRichPresenceUpdated) return;
  this->listener.OnRichPresenceUpdated(this->listener.userptr, (gog_ID)userID.ToUint64());
}

void OnRichPresenceRetrieveSuccess(GalaxyID userID) override {
  if(!this->listener.OnRichPresenceRetrieveSuccess) return;
  this->listener.OnRichPresenceRetrieveSuccess(this->listener.userptr, (gog_ID)userID.ToUint64());
}

void OnRichPresenceRetrieveFailure(GalaxyID userID, galaxy::api::IRichPresenceRetrieveListener::FailureReason failureReason) override {
  if(!this->listener.OnRichPresenceRetrieveFailure) return;
  this->listener.OnRichPresenceRetrieveFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FailureReason)failureReason);
}

void OnGameJoinRequested(GalaxyID userID, const char* connectionString) override {
  if(!this->listener.OnGameJoinRequested) return;
  this->listener.OnGameJoinRequested(this->listener.userptr, (gog_ID)userID.ToUint64(), (const char*)connectionString);
}

void OnGameInvitationReceived(GalaxyID userID, const char* connectionString) override {
  if(!this->listener.OnGameInvitationReceived) return;
  this->listener.OnGameInvitationReceived(this->listener.userptr, (gog_ID)userID.ToUint64(), (const char*)connectionString);
}

void OnInvitationSendSuccess(GalaxyID userID, const char* connectionString) override {
  if(!this->listener.OnInvitationSendSuccess) return;
  this->listener.OnInvitationSendSuccess(this->listener.userptr, (gog_ID)userID.ToUint64(), (const char*)connectionString);
}

void OnInvitationSendFailure(GalaxyID userID, const char* connectionString, galaxy::api::ISendInvitationListener::FailureReason failureReason) override {
  if(!this->listener.OnInvitationSendFailure) return;
  this->listener.OnInvitationSendFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (const char*)connectionString, (enum gog_SendInvitationFailureReason)failureReason);
}

void OnUserFindSuccess(const char* userSpecifier, GalaxyID userID) override {
  if(!this->listener.OnUserFindSuccess) return;
  this->listener.OnUserFindSuccess(this->listener.userptr, (const char*)userSpecifier, (gog_ID)userID.ToUint64());
}

void OnUserFindFailure(const char* userSpecifier, galaxy::api::IUserFindListener::FailureReason failureReason) override {
  if(!this->listener.OnUserFindFailure) return;
  this->listener.OnUserFindFailure(this->listener.userptr, (const char*)userSpecifier, (enum gog_UserFindFailureReason)failureReason);
}

void OnChatRoomWithUserRetrieveSuccess(GalaxyID userID, ChatRoomID chatRoomID) override {
  if(!this->listener.OnChatRoomWithUserRetrieveSuccess) return;
  this->listener.OnChatRoomWithUserRetrieveSuccess(this->listener.userptr, (gog_ID)userID.ToUint64(), (gog_ChatRoomID)chatRoomID);
}

void OnChatRoomWithUserRetrieveFailure(GalaxyID userID, galaxy::api::IChatRoomWithUserRetrieveListener::FailureReason failureReason) override {
  if(!this->listener.OnChatRoomWithUserRetrieveFailure) return;
  this->listener.OnChatRoomWithUserRetrieveFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_ChatUserRetrieveFailureReason)failureReason);
}

void OnChatRoomMessageSendSuccess(ChatRoomID chatRoomID, uint32_t sentMessageIndex, ChatMessageID messageID, uint32_t sendTime) override {
  if(!this->listener.OnChatRoomMessageSendSuccess) return;
  this->listener.OnChatRoomMessageSendSuccess(this->listener.userptr, (gog_ChatRoomID)chatRoomID, (uint32_t)sentMessageIndex, (gog_ChatMessageID)messageID, (uint32_t)sendTime);
}

void OnChatRoomMessageSendFailure(ChatRoomID chatRoomID, uint32_t sentMessageIndex, galaxy::api::IChatRoomMessageSendListener::FailureReason failureReason) override {
  if(!this->listener.OnChatRoomMessageSendFailure) return;
  this->listener.OnChatRoomMessageSendFailure(this->listener.userptr, (gog_ChatRoomID)chatRoomID, (uint32_t)sentMessageIndex, (enum gog_ChatMessageSendFailureReason)failureReason);
}

void OnChatRoomMessagesReceived(ChatRoomID chatRoomID, uint32_t messageCount, uint32_t longestMessageLenght) override {
  if(!this->listener.OnChatRoomMessagesReceived) return;
  this->listener.OnChatRoomMessagesReceived(this->listener.userptr, (gog_ChatRoomID)chatRoomID, (uint32_t)messageCount, (uint32_t)longestMessageLenght);
}

void OnChatRoomMessagesRetrieveSuccess(ChatRoomID chatRoomID, uint32_t messageCount, uint32_t longestMessageLenght) override {
  if(!this->listener.OnChatRoomMessagesRetrieveSuccess) return;
  this->listener.OnChatRoomMessagesRetrieveSuccess(this->listener.userptr, (gog_ChatRoomID)chatRoomID, (uint32_t)messageCount, (uint32_t)longestMessageLenght);
}

void OnChatRoomMessagesRetrieveFailure(ChatRoomID chatRoomID, galaxy::api::IChatRoomMessagesRetrieveListener::FailureReason failureReason) override {
  if(!this->listener.OnChatRoomMessagesRetrieveFailure) return;
  this->listener.OnChatRoomMessagesRetrieveFailure(this->listener.userptr, (gog_ChatRoomID)chatRoomID, (enum gog_ChatMessageRetrieveFailureReason)failureReason);
}

void OnLobbyList(uint32_t lobbyCount, LobbyListResult result) override {
  if(!this->listener.OnLobbyList) return;
  this->listener.OnLobbyList(this->listener.userptr, (uint32_t)lobbyCount, (enum gog_LobbyListResult)result);
}

void OnLobbyCreated(const GalaxyID& lobbyID, LobbyCreateResult result) override {
  if(!this->listener.OnLobbyCreated) return;
  this->listener.OnLobbyCreated(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (enum gog_LobbyCreateResult)result);
}

void OnLobbyEntered(const GalaxyID& lobbyID, LobbyEnterResult result) override {
  if(!this->listener.OnLobbyEntered) return;
  this->listener.OnLobbyEntered(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (enum gog_LobbyEnterResult)result);
}

void OnLobbyLeft(const GalaxyID& lobbyID, LobbyLeaveReason leaveReason) override {
  if(!this->listener.OnLobbyLeft) return;
  this->listener.OnLobbyLeft(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (enum gog_LobbyLeaveReason)leaveReason);
}

void OnLobbyDataUpdated(const GalaxyID& lobbyID, const GalaxyID& memberID) override {
  if(!this->listener.OnLobbyDataUpdated) return;
  this->listener.OnLobbyDataUpdated(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (gog_ID)memberID.ToUint64());
}

void OnLobbyDataUpdateSuccess(const GalaxyID& lobbyID) override {
  if(!this->listener.OnLobbyDataUpdateSuccess) return;
  this->listener.OnLobbyDataUpdateSuccess(this->listener.userptr, (gog_ID)lobbyID.ToUint64());
}

void OnLobbyDataUpdateFailure(const GalaxyID& lobbyID, galaxy::api::ILobbyDataUpdateListener::FailureReason failureReason) override {
  if(!this->listener.OnLobbyDataUpdateFailure) return;
  this->listener.OnLobbyDataUpdateFailure(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (enum gog_LobbyDataUpdateFailureReason)failureReason);
}

void OnLobbyMemberDataUpdateSuccess(const GalaxyID& lobbyID, const GalaxyID& memberID) override {
  if(!this->listener.OnLobbyMemberDataUpdateSuccess) return;
  this->listener.OnLobbyMemberDataUpdateSuccess(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (gog_ID)memberID.ToUint64());
}

void OnLobbyMemberDataUpdateFailure(const GalaxyID& lobbyID, const GalaxyID& memberID, galaxy::api::ILobbyMemberDataUpdateListener::FailureReason failureReason) override {
  if(!this->listener.OnLobbyMemberDataUpdateFailure) return;
  this->listener.OnLobbyMemberDataUpdateFailure(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (gog_ID)memberID.ToUint64(), (enum gog_LobbyDataUpdateFailureReason)failureReason);
}

void OnLobbyDataRetrieveSuccess(const GalaxyID& lobbyID) override {
  if(!this->listener.OnLobbyDataRetrieveSuccess) return;
  this->listener.OnLobbyDataRetrieveSuccess(this->listener.userptr, (gog_ID)lobbyID.ToUint64());
}

void OnLobbyDataRetrieveFailure(const GalaxyID& lobbyID, galaxy::api::ILobbyDataRetrieveListener::FailureReason failureReason) override {
  if(!this->listener.OnLobbyDataRetrieveFailure) return;
  this->listener.OnLobbyDataRetrieveFailure(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (enum gog_LobbyDataRetrieveFailureReason)failureReason);
}

void OnLobbyMemberStateChanged(const GalaxyID& lobbyID, const GalaxyID& memberID, LobbyMemberStateChange memberStateChange) override {
  if(!this->listener.OnLobbyMemberStateChanged) return;
  this->listener.OnLobbyMemberStateChanged(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (gog_ID)memberID.ToUint64(), (enum gog_LobbyMemberStateChange)memberStateChange);
}

void OnLobbyOwnerChanged(const GalaxyID& lobbyID, const GalaxyID& newOwnerID) override {
  if(!this->listener.OnLobbyOwnerChanged) return;
  this->listener.OnLobbyOwnerChanged(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (gog_ID)newOwnerID.ToUint64());
}

void OnLobbyMessageReceived(const GalaxyID& lobbyID, const GalaxyID& senderID, uint32_t messageID, uint32_t messageLength) override {
  if(!this->listener.OnLobbyMessageReceived) return;
  this->listener.OnLobbyMessageReceived(this->listener.userptr, (gog_ID)lobbyID.ToUint64(), (gog_ID)senderID.ToUint64(), (uint32_t)messageID, (uint32_t)messageLength);
}

void OnP2PPacketAvailable(uint32_t msgSize, uint8_t channel) override {
  if(!this->listener.OnP2PPacketAvailable) return;
  this->listener.OnP2PPacketAvailable(this->listener.userptr, (uint32_t)msgSize, (uint8_t)channel);
}

void OnNatTypeDetectionSuccess(NatType natType) override {
  if(!this->listener.OnNatTypeDetectionSuccess) return;
  this->listener.OnNatTypeDetectionSuccess(this->listener.userptr, (enum gog_NatType)natType);
}

void OnNatTypeDetectionFailure() override {
  if(!this->listener.OnNatTypeDetectionFailure) return;
  this->listener.OnNatTypeDetectionFailure(this->listener.userptr);
}

void OnUserStatsAndAchievementsRetrieveSuccess(GalaxyID userID) override {
  if(!this->listener.OnUserStatsAndAchievementsRetrieveSuccess) return;
  this->listener.OnUserStatsAndAchievementsRetrieveSuccess(this->listener.userptr, (gog_ID)userID.ToUint64());
}

void OnUserStatsAndAchievementsRetrieveFailure(GalaxyID userID, galaxy::api::IUserStatsAndAchievementsRetrieveListener::FailureReason failureReason) override {
  if(!this->listener.OnUserStatsAndAchievementsRetrieveFailure) return;
  this->listener.OnUserStatsAndAchievementsRetrieveFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FailureReason)failureReason);
}

void OnUserStatsAndAchievementsStoreSuccess() override {
  if(!this->listener.OnUserStatsAndAchievementsStoreSuccess) return;
  this->listener.OnUserStatsAndAchievementsStoreSuccess(this->listener.userptr);
}

void OnUserStatsAndAchievementsStoreFailure(galaxy::api::IStatsAndAchievementsStoreListener::FailureReason failureReason) override {
  if(!this->listener.OnUserStatsAndAchievementsStoreFailure) return;
  this->listener.OnUserStatsAndAchievementsStoreFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
}

void OnAchievementUnlocked(const char* name) override {
  if(!this->listener.OnAchievementUnlocked) return;
  this->listener.OnAchievementUnlocked(this->listener.userptr, (const char*)name);
}

void OnLeaderboardsRetrieveSuccess() override {
  if(!this->listener.OnLeaderboardsRetrieveSuccess) return;
  this->listener.OnLeaderboardsRetrieveSuccess(this->listener.userptr);
}

void OnLeaderboardsRetrieveFailure(galaxy::api::ILeaderboardsRetrieveListener::FailureReason failureReason) override {
  if(!this->listener.OnLeaderboardsRetrieveFailure) return;
  this->listener.OnLeaderboardsRetrieveFailure(this->listener.userptr, (enum gog_FailureReason)failureReason);
}

void OnLeaderboardEntriesRetrieveSuccess(const char* name, uint32_t entryCount) override {
  if(!this->listener.OnLeaderboardEntriesRetrieveSuccess) return;
  this->listener.OnLeaderboardEntriesRetrieveSuccess(this->listener.userptr, (const char*)name, (uint32_t)entryCount);
}

void OnLeaderboardEntriesRetrieveFailure(const char* name, galaxy::api::ILeaderboardEntriesRetrieveListener::FailureReason failureReason) override {
  if(!this->listener.OnLeaderboardEntriesRetrieveFailure) return;
  this->listener.OnLeaderboardEntriesRetrieveFailure(this->listener.userptr, (const char*)name, (enum gog_FailureReason)failureReason);
}

void OnLeaderboardScoreUpdateSuccess(const char* name, int32_t score, uint32_t oldRank, uint32_t newRank) override {
  if(!this->listener.OnLeaderboardScoreUpdateSuccess) return;
  this->listener.OnLeaderboardScoreUpdateSuccess(this->listener.userptr, (const char*)name, (int32_t)score, (uint32_t)oldRank, (uint32_t)newRank);
}

void OnLeaderboardScoreUpdateFailure(const char* name, int32_t score, galaxy::api::ILeaderboardScoreUpdateListener::FailureReason failureReason) override {
  if(!this->listener.OnLeaderboardScoreUpdateFailure) return;
  this->listener.OnLeaderboardScoreUpdateFailure(this->listener.userptr, (const char*)name, (int32_t)score, (enum gog_LeaderboardScoreUpdateFailureReason)failureReason);
}

void OnLeaderboardRetrieveSuccess(const char* name) override {
  if(!this->listener.OnLeaderboardRetrieveSuccess) return;
  this->listener.OnLeaderboardRetrieveSuccess(this->listener.userptr, (const char*)name);
}

void OnLeaderboardRetrieveFailure(const char* name, galaxy::api::ILeaderboardRetrieveListener::FailureReason failureReason) override {
  if(!this->listener.OnLeaderboardRetrieveFailure) return;
  this->listener.OnLeaderboardRetrieveFailure(this->listener.userptr, (const char*)name, (enum gog_FailureReason)failureReason);
}

void OnUserTimePlayedRetrieveSuccess(GalaxyID userID) override {
  if(!this->listener.OnUserTimePlayedRetrieveSuccess) return;
  this->listener.OnUserTimePlayedRetrieveSuccess(this->listener.userptr, (gog_ID)userID.ToUint64());
}

void OnUserTimePlayedRetrieveFailure(GalaxyID userID, galaxy::api::IUserTimePlayedRetrieveListener::FailureReason failureReason) override {
  if(!this->listener.OnUserTimePlayedRetrieveFailure) return;
  this->listener.OnUserTimePlayedRetrieveFailure(this->listener.userptr, (gog_ID)userID.ToUint64(), (enum gog_FailureReason)failureReason);
}

void OnOverlayVisibilityChanged(bool overlayVisible) override {
  if(!this->listener.OnOverlayVisibilityChanged) return;
  this->listener.OnOverlayVisibilityChanged(this->listener.userptr, (bool)overlayVisible);
}

void OnOverlayStateChanged(OverlayState overlayState) override {
  if(!this->listener.OnOverlayStateChanged) return;
  this->listener.OnOverlayStateChanged(this->listener.userptr, (enum gog_OverlayState)overlayState);
}

void OnNotificationReceived(NotificationID notificationID, uint32_t typeLength, uint32_t contentSize) override {
  if(!this->listener.OnNotificationReceived) return;
  this->listener.OnNotificationReceived(this->listener.userptr, (gog_NotificationID)notificationID, (uint32_t)typeLength, (uint32_t)contentSize);
}

void OnConnectionStateChange(GogServicesConnectionState connectionState) override {
  if(!this->listener.OnConnectionStateChange) return;
  this->listener.OnConnectionStateChange(this->listener.userptr, (enum gog_ServicesConnectionState)connectionState);
}

void OnDlcCheckSuccess(ProductID productId, bool isOwned) override {
  if(!this->listener.OnDlcCheckSuccess) return;
  this->listener.OnDlcCheckSuccess(this->listener.userptr, (gog_ProductID)productId, (bool)isOwned);
}

void OnDlcCheckFailure(ProductID productId, galaxy::api::IIsDlcOwnedListener::FailureReason failureReason) override {
  if(!this->listener.OnDlcCheckFailure) return;
  this->listener.OnDlcCheckFailure(this->listener.userptr, (gog_ProductID)productId, (enum gog_DlcCheckFailureReason)failureReason);
}

void OnFileShareSuccess(const char* fileName, SharedFileID sharedFileID) override {
  if(!this->listener.OnFileShareSuccess) return;
  this->listener.OnFileShareSuccess(this->listener.userptr, (const char*)fileName, (gog_SharedFileID)sharedFileID);
}

void OnFileShareFailure(const char* fileName, galaxy::api::IFileShareListener::FailureReason failureReason) override {
  if(!this->listener.OnFileShareFailure) return;
  this->listener.OnFileShareFailure(this->listener.userptr, (const char*)fileName, (enum gog_FailureReason)failureReason);
}

void OnSharedFileDownloadSuccess(SharedFileID sharedFileID, const char* fileName) override {
  if(!this->listener.OnSharedFileDownloadSuccess) return;
  this->listener.OnSharedFileDownloadSuccess(this->listener.userptr, (gog_SharedFileID)sharedFileID, (const char*)fileName);
}

void OnSharedFileDownloadFailure(SharedFileID sharedFileID, galaxy::api::ISharedFileDownloadListener::FailureReason failureReason) override {
  if(!this->listener.OnSharedFileDownloadFailure) return;
  this->listener.OnSharedFileDownloadFailure(this->listener.userptr, (gog_SharedFileID)sharedFileID, (enum gog_FailureReason)failureReason);
}

void OnConnectionOpenSuccess(const char* connectionString, ConnectionID connectionID) override {
  if(!this->listener.OnConnectionOpenSuccess) return;
  this->listener.OnConnectionOpenSuccess(this->listener.userptr, (const char*)connectionString, (gog_ConnectionID)connectionID);
}

void OnConnectionOpenFailure(const char* connectionString, galaxy::api::IConnectionOpenListener::FailureReason failureReason) override {
  if(!this->listener.OnConnectionOpenFailure) return;
  this->listener.OnConnectionOpenFailure(this->listener.userptr, (const char*)connectionString, (enum gog_ConnectionOpenFailureReason)failureReason);
}

void OnConnectionClosed(ConnectionID connectionID, CloseReason closeReason) override {
  if(!this->listener.OnConnectionClosed) return;
  this->listener.OnConnectionClosed(this->listener.userptr, (gog_ConnectionID)connectionID, (enum gog_CloseReason)closeReason);
}

void OnConnectionDataReceived(ConnectionID connectionID, uint32_t dataSize) override {
  if(!this->listener.OnConnectionDataReceived) return;
  this->listener.OnConnectionDataReceived(this->listener.userptr, (gog_ConnectionID)connectionID, (uint32_t)dataSize);
}

void OnTelemetryEventSendSuccess(const char* eventType, uint32_t sentEventIndex) override {
  if(!this->listener.OnTelemetryEventSendSuccess) return;
  this->listener.OnTelemetryEventSendSuccess(this->listener.userptr, (const char*)eventType, (uint32_t)sentEventIndex);
}

void OnTelemetryEventSendFailure(const char* eventType, uint32_t sentEventIndex, galaxy::api::ITelemetryEventSendListener::FailureReason failureReason) override {
  if(!this->listener.OnTelemetryEventSendFailure) return;
  this->listener.OnTelemetryEventSendFailure(this->listener.userptr, (const char*)eventType, (uint32_t)sentEventIndex, (enum gog_TelemetryEventFailureReason)failureReason);
}

void OnGetFileListSuccess(uint32_t fileCount, uint32_t quota, uint32_t quotaUsed) override {
  if(!this->listener.OnGetFileListSuccess) return;
  this->listener.OnGetFileListSuccess(this->listener.userptr, (uint32_t)fileCount, (uint32_t)quota, (uint32_t)quotaUsed);
}

void OnGetFileListFailure(galaxy::api::ICloudStorageGetFileListListener::FailureReason failureReason) override {
  if(!this->listener.OnGetFileListFailure) return;
  this->listener.OnGetFileListFailure(this->listener.userptr, (enum gog_GetFileListFailureReason)failureReason);
}

void OnGetFileSuccess(
				const char* container, 
				const char* name, 
				uint32_t fileSize, 
				SavegameType savegameType, 
				const char* savegameID
			) override {
  if(!this->listener.OnGetFileSuccess) return;
  this->listener.OnGetFileSuccess(this->listener.userptr, (const char*)container, (const char*)name, (uint32_t)fileSize, (enum gog_SavegameType)savegameType, (const char*)savegameID
			);
}

void OnGetFileFailure(const char* container, const char* name, galaxy::api::ICloudStorageGetFileListener::FailureReason failureReason) override {
  if(!this->listener.OnGetFileFailure) return;
  this->listener.OnGetFileFailure(this->listener.userptr, (const char*)container, (const char*)name, (enum gog_GetFileFailureReason)failureReason);
}

void OnPutFileSuccess(const char* container, const char* name) override {
  if(!this->listener.OnPutFileSuccess) return;
  this->listener.OnPutFileSuccess(this->listener.userptr, (const char*)container, (const char*)name);
}

void OnPutFileFailure(const char* container, const char* name, galaxy::api::ICloudStoragePutFileListener::FailureReason failureReason) override {
  if(!this->listener.OnPutFileFailure) return;
  this->listener.OnPutFileFailure(this->listener.userptr, (const char*)container, (const char*)name, (enum gog_PutFileFailureReason)failureReason);
}

void OnDeleteFileSuccess(const char* container, const char* name) override {
  if(!this->listener.OnDeleteFileSuccess) return;
  this->listener.OnDeleteFileSuccess(this->listener.userptr, (const char*)container, (const char*)name);
}

void OnDeleteFileFailure(const char* container, const char* name, galaxy::api::ICloudStorageDeleteFileListener::FailureReason failureReason) override {
  if(!this->listener.OnDeleteFileFailure) return;
  this->listener.OnDeleteFileFailure(this->listener.userptr, (const char*)container, (const char*)name, (enum gog_DeleteFileFailureReason)failureReason);
}
};

gog_Interface gog_MakeListener(struct gog_listener *listener){
  Listener *list = new Listener();
  memcpy(&list->listener, listener, sizeof(struct gog_listener));
  return (gog_Interface)list;
}

void gog_FreeListener(gog_Interface listener){
  delete (Listener*)listener;
}