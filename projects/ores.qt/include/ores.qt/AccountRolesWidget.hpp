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
#include <QComboBox>
#include <QListWidget>
#include <QToolButton>
#include <QGroupBox>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.iam/domain/role.hpp"

namespace ores::qt {

/**
 * @brief Widget for managing roles assigned to an account.
 *
 * Changes are staged locally and only committed when the parent dialog saves.
 */
class AccountRolesWidget : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.account_roles_widget";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit AccountRolesWidget(QWidget* parent = nullptr);
    ~AccountRolesWidget() override = default;

    void setClientManager(ClientManager* clientManager);
    void setAccountId(const boost::uuids::uuid& accountId);

    /**
     * @brief Load roles. If accountId is set, also fetches assigned roles.
     * If accountId is nil (create mode), only available roles are loaded.
     */
    void load();

    void setReadOnly(bool readOnly);

    [[nodiscard]] bool hasPendingChanges() const;
    [[nodiscard]] const std::vector<boost::uuids::uuid>& pendingAdds() const;
    [[nodiscard]] const std::vector<boost::uuids::uuid>& pendingRemoves() const;

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& message);
    void roleListChanged();

private slots:
    void onAssignRoleClicked();
    void onRevokeRoleClicked();
    void onRoleSelectionChanged();

private:
    void setupUi();
    void updateButtonStates();
    void refreshRolesList();

    QGroupBox*   groupBox_;
    QListWidget* rolesList_;
    QComboBox*   roleCombo_;
    QToolButton* assignButton_;
    QToolButton* revokeButton_;

    ClientManager*     clientManager_ = nullptr;
    boost::uuids::uuid accountId_;
    bool               isReadOnly_{false};

    std::vector<iam::domain::role> assignedRoles_;
    std::vector<iam::domain::role> allRoles_;
    std::vector<boost::uuids::uuid> pendingAdds_;
    std::vector<boost::uuids::uuid> pendingRemoves_;
};

}

#endif
