/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_CLIENT_ACCOUNT_MODEL_HPP
#define ORES_QT_CLIENT_ACCOUNT_MODEL_HPP

#include <vector>
#include <optional>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/RecencyPulseManager.hpp"
#include "ores.qt/RecencyTracker.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.iam/domain/account.hpp"
#include "ores.iam/domain/login_info.hpp"

namespace ores::qt {

/**
 * @brief Enum representing account login status buckets.
 *
 * Used to categorize accounts by their login recency for display
 * with different visual treatments (badge colors).
 */
enum class LoginStatus {
    Never,    // Never logged in (no login_info or epoch last_login)
    LongAgo,  // Logged in more than 30 days ago
    Recent,   // Logged in within the last 30 days
    Online    // Currently logged in
};

/**
 * @brief Composite structure combining account with its login status.
 *
 * This struct joins account domain data with login_info data for display
 * purposes in the Qt UI. The login_info is optional since not all accounts
 * may have corresponding login_info records (e.g., newly created accounts
 * that have never logged in).
 */
struct AccountWithLoginInfo {
    iam::domain::account account;
    std::optional<iam::domain::login_info> loginInfo;
};

/**
 * @brief Model for displaying accounts fetched from the server via client.
 *
 * This model extends QAbstractTableModel and fetches account data
 * asynchronously using the ores.comms client instead of direct database access.
 */
class ClientAccountModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_account_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ClientAccountModel(ClientManager* clientManager,
                                QObject* parent = nullptr);
    ~ClientAccountModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Refresh account data from server asynchronously.
     *
     * This method initiates an async request to fetch accounts.
     * The model will emit dataChanged() when the fetch completes.
     * When replace is true, existing data is cleared before loading.
     * When false, new data is appended (for pagination).
     *
     * @param replace If true, replace existing data; if false, append.
     */
    void refresh(bool replace = true);

    /**
     * @brief Load a specific page of account data.
     *
     * Used for pagination navigation. Replaces current data with the
     * requested page.
     *
     * @param offset Number of records to skip
     * @param limit Number of records to fetch
     */
    void load_page(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Check if more data can be fetched from the server.
     *
     * @return true if there are more records available on the server
     */
    bool canFetchMore(const QModelIndex& parent = QModelIndex()) const override;

    /**
     * @brief Fetch the next page of data from the server.
     *
     * This is called automatically by Qt views when scrolling approaches
     * the end of currently loaded data.
     */
    void fetchMore(const QModelIndex& parent = QModelIndex()) override;

    /**
     * @brief Get account with login info at the specified row.
     *
     * @param row The row index.
     * @return The account with login info, or nullptr if row is invalid.
     */
    const AccountWithLoginInfo* getAccountWithLoginInfo(int row) const;

    /**
     * @brief Get account at the specified row (for backward compatibility).
     *
     * @param row The row index.
     * @return The account object, or nullptr if row is invalid.
     */
    const iam::domain::account* getAccount(int row) const;

    /**
     * @brief Get all accounts with their login info.
     *
     * @return A vector containing all current accounts with login info.
     */
    std::vector<AccountWithLoginInfo> getAccountsWithLoginInfo() const;

    /**
     * @brief Get the page size used for pagination.
     *
     * @return The number of records fetched per page.
     */
    std::uint32_t page_size() const { return page_size_; }

    /**
     * @brief Set the page size for pagination.
     *
     * @param size The number of records to fetch per page (1-1000).
     */
    void set_page_size(std::uint32_t size);

    /**
     * @brief Get the total number of records available on the server.
     *
     * @return Total available record count.
     */
    std::uint32_t total_available_count() const { return total_available_count_; }

signals:
    /**
     * @brief Emitted when data has been successfully loaded.
     */
    void dataLoaded();

    /**
     * @brief Emitted when an error occurs during data loading.
     */
    void loadError(const QString& error_message, const QString& details = {});

private slots:
    void onAccountsLoaded();
    void onPulseStateChanged(bool isOn);
    void onPulsingComplete();

private:
    /**
     * @brief Calculate foreground color based on how recent the account's recorded_at date is.
     *
     * Returns a decaying highlight color for accounts with the most recent recorded_at dates.
     * The color fades from highlight to transparent over the decay duration.
     *
     * @param username The account's username to check for recency.
     * @return QVariant containing QColor for foreground, or invalid QVariant if no color.
     */
    QVariant recency_foreground_color(const std::string& username) const;

    /**
     * @brief Enumeration of table columns for type-safe column access.
     *
     * Using an enum instead of magic numbers makes the code self-documenting
     * and easier to refactor when columns are added, removed, or reordered.
     * Note: IsAdmin removed - admin privileges are now managed via RBAC.
     */
    enum Column {
        Username,
        AccountType,
        Email,
        Status,     // Login status: Never, LongAgo, Recent, Online
        Locked,
        Version,
        RecordedBy,
        RecordedAt,
        ColumnCount  // Must be last - represents total number of columns
    };

    /**
     * @brief Calculate login status from login_info.
     *
     * @param loginInfo Optional login info for the account.
     * @return LoginStatus bucket based on login recency.
     */
    static LoginStatus calculateLoginStatus(
        const std::optional<iam::domain::login_info>& loginInfo);

    struct FetchResult {
        bool success;
        std::vector<iam::domain::account> accounts;
        std::vector<iam::domain::login_info> loginInfos;
        std::uint32_t total_available_count;
        QString error_message;
        QString error_details;
    };

    using FutureWatcherResult = FetchResult;

    ClientManager* clientManager_;
    std::vector<AccountWithLoginInfo> accounts_;
    QFutureWatcher<FutureWatcherResult>* watcher_;
    std::uint32_t page_size_{100};
    std::uint32_t total_available_count_{0};
    bool is_fetching_{false};

    // Recency highlighting
    using AccountKeyExtractor = std::string(*)(const iam::domain::account&);
    RecencyTracker<iam::domain::account, AccountKeyExtractor> recencyTracker_;
    RecencyPulseManager* pulseManager_;

    /**
     * @brief Internal method to fetch accounts with specific offset and limit.
     */
    void fetch_accounts(std::uint32_t offset, std::uint32_t limit);
};

}

#endif
