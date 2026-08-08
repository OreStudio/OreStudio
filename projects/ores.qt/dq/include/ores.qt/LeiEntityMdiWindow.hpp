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
#ifndef ORES_QT_LEI_ENTITY_MDI_WINDOW_HPP
#define ORES_QT_LEI_ENTITY_MDI_WINDOW_HPP

#include "ores.dq.api/domain/lei_entity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientLeiEntityModel.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/PaginationWidget.hpp"
#include <QSortFilterProxyModel>
#include <QTableView>
#include <QToolBar>

namespace ores::qt {


/**
 * @brief MDI window for displaying and managing LEI entities.
 *
 * Provides a table view of LEI entities with toolbar actions
 * for reload, add, edit, delete, and viewing history.
 */
class LeiEntityMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.lei_entity_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit LeiEntityMdiWindow(ClientManager* clientManager,
                                const QString& username,
                                QWidget* parent = nullptr);
    ~LeiEntityMdiWindow() override = default;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    // Extra signal declarations seam: a future
    // :implements 67D24D2F-2D98-49EB-9A1D-32F1D8BFA76A block is expected
    // to declare any entity-specific signals (e.g. a cross-navigation
    // request to a related entity's list window) — see
    // paste_blocks_in_codegen.org. Left empty when no entity implements
    // this kind.

public slots:

protected:
    void doReload() override;

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onSelectionChanged();
    void onDoubleClicked(const QModelIndex& index);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh LEI entities");
    }

private:
    void setupUi();
    void setupToolbar();
    void setupTable();
    void setupConnections();

    ClientManager* clientManager_;
    QString username_;

    QToolBar* toolbar_;
    QTableView* tableView_;
    ClientLeiEntityModel* model_;
    QSortFilterProxyModel* proxyModel_;
    PaginationWidget* paginationWidget_;

    // Toolbar actions
    QAction* reloadAction_;
};

}

#endif
