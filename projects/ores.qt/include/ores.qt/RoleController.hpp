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
#ifndef ORES_QT_ROLE_CONTROLLER_HPP
#define ORES_QT_ROLE_CONTROLLER_HPP

#include <QPointer>
#include <QList>
#include <QDateTime>
#include "ores.qt/EntityController.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.iam/domain/role.hpp"

namespace ores::qt {

class DetachableMdiSubWindow;

/**
 * @brief Controller managing all role-related windows and operations.
 *
 * The RoleController encapsulates all role management functionality,
 * including:
 *
 * - Role list window showing all roles in the system
 * - Role detail dialogs for viewing role details and permissions
 * - Window lifecycle management (creation, tracking, cleanup)
 */
class RoleController : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.role_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs the role controller.
     *
     * @param mainWindow Parent main window (for dialog ownership)
     * @param mdiArea MDI area where windows will be displayed
     * @param clientManager Client manager for network operations
     * @param username Username of logged-in user
     * @param allDetachableWindows Reference to MainWindow's window list
     * @param parent QObject parent (for Qt ownership)
     */
    explicit RoleController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        const QString& username,
        QList<DetachableMdiSubWindow*>& allDetachableWindows,
        QObject* parent = nullptr);

    /**
     * @brief Destroys the role controller.
     */
    ~RoleController() override;

    /**
     * @brief Shows the role list window.
     *
     * If the window already exists, brings it to front. Otherwise, creates a
     * new role list window displaying all roles from the server.
     */
    void showListWindow() override;

    /**
     * @brief Closes all windows managed by this controller.
     */
    void closeAllWindows() override;

private slots:
    /**
     * @brief Handles request to show role details.
     *
     * Creates and displays a role detail dialog for viewing a role's
     * information and assigned permissions.
     *
     * @param role The role to display
     */
    void onShowRoleDetails(const iam::domain::role& role);

    /**
     * @brief Handles role change notifications from the server.
     *
     * @param eventType The event type name
     * @param timestamp When the event occurred
     */
    void onNotificationReceived(const QString& eventType, const QDateTime& timestamp);

private:
    /**
     * @brief Marks the role list as stale if it exists.
     */
    void markRoleListAsStale();

    /**
     * @brief Reference to MainWindow's list of all detachable windows.
     */
    QList<DetachableMdiSubWindow*>& allDetachableWindows_;

    /**
     * @brief Weak pointer to the role list window.
     */
    QPointer<DetachableMdiSubWindow> roleListWindow_;
};

}

#endif
