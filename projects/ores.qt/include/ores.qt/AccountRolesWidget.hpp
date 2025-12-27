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
#ifndef ORES_QT_ACCOUNT_ROLES_WIDGET_HPP
#define ORES_QT_ACCOUNT_ROLES_WIDGET_HPP

#include <QWidget>
#include <QListWidget>
#include <QToolButton>
#include <QGroupBox>
#include <vector>
#include <functional>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.iam/domain/role.hpp"

namespace ores::qt {

/**
 * @brief Widget for managing roles assigned to an account.
 *
 * This widget displays the roles currently assigned to an account
 * and allows assigning/revoking roles via buttons.
 */
class AccountRolesWidget : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.account_roles_widget";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit AccountRolesWidget(QWidget* parent = nullptr);
    ~AccountRolesWidget() override = default;

    /**
     * @brief Sets the client manager for making requests.
     */
    void setClientManager(ClientManager* clientManager);

    /**
     * @brief Sets the account ID to manage roles for.
     */
    void setAccountId(const boost::uuids::uuid& accountId);

    /**
     * @brief Loads the roles for the current account.
     */
    void loadRoles();

    /**
     * @brief Sets the widget to read-only mode.
     */
    void setReadOnly(bool readOnly);

    /**
     * @brief Gets the currently assigned role IDs.
     */
    std::vector<boost::uuids::uuid> getAssignedRoleIds() const;

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& message);
    void rolesChanged();

private slots:
    void onAssignRoleClicked();
    void onRevokeRoleClicked();
    void onRoleSelectionChanged();

private:
    void updateButtonStates();
    void refreshRolesList();

    /**
     * @brief Executes an async role operation with common success/error handling.
     *
     * @param requestFunc Function that performs the actual request
     * @param roleName Name of the role for status messages
     * @param successMessage Message format for success (role name appended)
     * @param errorTitle Title for error dialog
     */
    void executeRoleOperation(
        std::function<std::pair<bool, std::string>()> requestFunc,
        const std::string& roleName,
        const QString& successMessage,
        const QString& errorTitle);

private:
    QGroupBox* groupBox_;
    QListWidget* rolesList_;
    QToolButton* assignButton_;
    QToolButton* revokeButton_;

    ClientManager* clientManager_;
    boost::uuids::uuid accountId_;
    std::vector<iam::domain::role> assignedRoles_;
    std::vector<iam::domain::role> allRoles_;
    bool isReadOnly_{false};
};

}

#endif
