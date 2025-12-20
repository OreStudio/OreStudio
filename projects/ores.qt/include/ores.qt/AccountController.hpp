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
#ifndef ORES_QT_ACCOUNT_CONTROLLER_HPP
#define ORES_QT_ACCOUNT_CONTROLLER_HPP

#include <optional>
#include <QPointer>
#include <QList>
#include <QDateTime>
#include "ores.qt/EntityController.hpp"
#include "ores.qt/ClientAccountModel.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.iam/domain/account.hpp"

namespace ores::qt {

class DetachableMdiSubWindow;

/**
 * @brief Controller managing all account-related windows and operations.
 *
 * The AccountController encapsulates all account management functionality,
 * including:
 *
 * - Account list window showing all user accounts in the system
 * - Account detail dialogs for creating/editing accounts
 * - Window lifecycle management (creation, tracking, cleanup)
 *
 * This controller follows the entity controller pattern where MainWindow
 * delegates all account operations to this controller, keeping the main window
 * clean and entity-agnostic.
 *
 * @note Account management is only available to admin users. The MainWindow
 * should check ClientManager::isAdmin() before showing account-related menus.
 */
class AccountController : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.account_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs the account controller.
     *
     * @param mainWindow Parent main window (for dialog ownership)
     * @param mdiArea MDI area where windows will be displayed
     * @param clientManager Client manager for network operations
     * @param username Username of logged-in user (for audit trails)
     * @param allDetachableWindows Reference to MainWindow's window list
     * for detach/reattach operations
     * @param parent QObject parent (for Qt ownership)
     */
    explicit AccountController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        const QString& username,
        QList<DetachableMdiSubWindow*>& allDetachableWindows,
        QObject* parent = nullptr);

    /**
     * @brief Destroys the account controller.
     *
     * All windows owned by this controller are automatically cleaned up through
     * Qt's parent-child ownership.
     */
    ~AccountController() override;

    /**
     * @brief Shows the account list window.
     *
     * If the window already exists, brings it to front. Otherwise, creates a
     * new account list window displaying all accounts from the server.
     *
     * @note Inherited from EntityController
     */
    void showListWindow() override;

    /**
     * @brief Closes all windows managed by this controller.
     *
     * Called when disconnecting from the server to clean up all account
     * windows before destroying the controller.
     *
     * @note Inherited from EntityController
     */
    void closeAllWindows() override;

private slots:
    /**
     * @brief Handles request to add a new account.
     *
     * Creates and displays an account detail dialog for entering a new
     * account. Connected to the account list window's add button.
     */
    void onAddNewRequested();

    /**
     * @brief Handles request to show account details.
     *
     * Creates and displays an account detail dialog for viewing/editing an
     * existing account. Connected to the account list window's edit button
     * and double-click.
     *
     * @param accountWithLoginInfo The account with login info to display/edit
     */
    void onShowAccountDetails(const AccountWithLoginInfo& accountWithLoginInfo);

    /**
     * @brief Handles request to show account history.
     *
     * Creates and displays an account history dialog for viewing version
     * history of an account.
     *
     * @param username The username of the account to show history for
     */
    void onShowAccountHistory(const QString& username);

    /**
     * @brief Handles request to open a historical account version in read-only mode.
     *
     * Creates an account detail dialog showing the historical version with
     * all fields read-only and a revert button available.
     *
     * @param account The account data at the historical version
     * @param versionNumber The version number being viewed
     */
    void onOpenAccountVersion(const accounts::domain::account& account, int versionNumber);

    /**
     * @brief Handles request to revert an account to a historical version.
     *
     * Saves the account data from the historical version, creating a new
     * version with that data.
     *
     * @param account The account data to revert to
     */
    void onRevertAccount(const accounts::domain::account& account);

    /**
     * @brief Handles account change notifications from the server.
     *
     * Called when a notification is received indicating accounts have changed.
     * Marks the account list window as stale to indicate data needs refreshing.
     *
     * @param eventType The event type name
     * @param timestamp When the event occurred
     */
    void onNotificationReceived(const QString& eventType, const QDateTime& timestamp);

private:
    /**
     * @brief Creates and shows an account detail window.
     *
     * This helper consolidates the common logic for creating account detail
     * dialogs, whether for new accounts or editing existing ones.
     *
     * @param accountWithLoginInfo Optional account with login info to edit.
     *        If nullopt, creates a new account.
     */
    void showDetailWindow(const std::optional<AccountWithLoginInfo>& accountWithLoginInfo);

    /**
     * @brief Marks the account list as stale if it exists.
     *
     * Helper method to reduce code duplication in signal handlers.
     */
    void markAccountListAsStale();

    /**
     * @brief Reference to MainWindow's list of all detachable windows.
     *
     * Windows created by this controller are added to this list so they
     * can participate in detach/reattach operations and appear in the
     * Window menu.
     */
    QList<DetachableMdiSubWindow*>& allDetachableWindows_;

    /**
     * @brief Weak pointer to the account list window.
     *
     * Uses QPointer to automatically become null if the window is closed
     * externally. This allows reusing the existing window if it exists, or
     * creating a new one if it was closed.
     */
    QPointer<DetachableMdiSubWindow> accountListWindow_;
};

}

#endif
