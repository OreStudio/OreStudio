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
#ifndef ORES_QT_TENANT_MDI_WINDOW_HPP
#define ORES_QT_TENANT_MDI_WINDOW_HPP

#include <QTableView>
#include <QVBoxLayout>
#include <QToolBar>
#include <QSortFilterProxyModel>
#include <memory>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientTenantModel.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief MDI window for displaying and managing tenants.
 *
 * This window provides functionality for viewing, creating, editing, and
 * deleting tenants. Tenants are the core multi-tenancy entities representing
 * isolated organisations.
 */
class TenantMdiWindow : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.tenant_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit TenantMdiWindow(ClientManager* clientManager,
                             const QString& username,
                             QWidget* parent = nullptr);
    ~TenantMdiWindow() override;

    ClientTenantModel* tenantModel() const { return tenantModel_.get(); }

    QSize sizeHint() const override { return QSize(900, 500); }

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void selectionChanged(int selection_count);
    void addNewRequested();
    void showTenantDetails(const iam::domain::tenant& tenant);
    void showTenantHistory(const QString& code);
    void tenantDeleted(const QString& code);

public slots:
    void reload() override;
    void addNew();
    void editSelected();
    void deleteSelected();
    void viewHistorySelected();

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh tenants");
    }

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onRowDoubleClicked(const QModelIndex& index);
    void onSelectionChanged();
    void onConnectionStateChanged();

private:
    void updateActionStates();
    void setupReloadAction();

    QVBoxLayout* verticalLayout_;
    QTableView* tenantTableView_;
    QToolBar* toolBar_;

    QAction* reloadAction_;
    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* historyAction_;

    std::unique_ptr<ClientTenantModel> tenantModel_;
    QSortFilterProxyModel* proxyModel_;
    ClientManager* clientManager_;
    QString username_;
};

}

#endif
