/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_TENANT_CONTROLLER_HPP
#define ORES_QT_TENANT_CONTROLLER_HPP

#include <QPointer>
#include <QDateTime>
#include "ores.qt/EntityController.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.iam/domain/tenant.hpp"

namespace ores::qt {

class DetachableMdiSubWindow;
class TenantMdiWindow;

/**
 * @brief Controller managing all tenant-related windows and operations.
 *
 * The TenantController encapsulates all tenant management functionality,
 * including:
 *
 * - Tenant list window showing all tenants in the system
 * - Tenant detail dialogs for creating/editing tenants
 * - Tenant history dialogs for viewing version history
 * - Window lifecycle management (creation, tracking, cleanup)
 *
 * This controller follows the entity controller pattern where MainWindow
 * delegates all tenant operations to this controller.
 *
 * @note Tenant management is only available to admin users.
 */
class TenantController : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.tenant_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs the tenant controller.
     *
     * @param mainWindow Parent main window (for dialog ownership)
     * @param mdiArea MDI area where windows will be displayed
     * @param clientManager Client manager for network operations
     * @param username Username of logged-in user (for audit trails)
     * @param parent QObject parent (for Qt ownership)
     */
    explicit TenantController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        const QString& username,
        QObject* parent = nullptr);

    ~TenantController() override;

    void showListWindow() override;
    void closeAllWindows() override;
    void reloadListWindow() override;

protected:
    EntityListMdiWindow* listWindow() const override;

private slots:
    void onAddNewRequested();
    void onShowTenantDetails(const iam::domain::tenant& tenant);
    void onShowTenantHistory(const QString& code);
    void onTenantDeleted(const QString& code);
    void onOpenTenantVersion(const iam::domain::tenant& tenant, int versionNumber);
    void onRevertTenant(const iam::domain::tenant& tenant);

private:
    void showDetailWindow(const iam::domain::tenant* tenant,
                          bool createMode, bool readOnly = false,
                          int versionNumber = 0);

    QPointer<TenantMdiWindow> listWindow_;
    QPointer<DetachableMdiSubWindow> listMdiSubWindow_;
};

}

#endif
