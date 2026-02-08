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
#ifndef ORES_QT_PARTY_MDI_WINDOW_HPP
#define ORES_QT_PARTY_MDI_WINDOW_HPP

#include <QToolBar>
#include <QTableView>
#include <QSortFilterProxyModel>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientPartyModel.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/party.hpp"

namespace ores::qt {

/**
 * @brief MDI window for displaying and managing parties.
 *
 * Provides a table view of parties with toolbar actions
 * for reload, add, edit, delete, and viewing history.
 */
class PartyMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.party_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PartyMdiWindow(
        ClientManager* clientManager,
        const QString& username,
        QWidget* parent = nullptr);
    ~PartyMdiWindow() override = default;

    QSize sizeHint() const override;

public slots:
    void reload() override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void showPartyDetails(const refdata::domain::party& party);
    void addNewRequested();
    void partyDeleted(const QString& code);
    void showPartyHistory(const refdata::domain::party& party);

public slots:
    void addNew();
    void editSelected();
    void deleteSelected();
    void viewHistorySelected();

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onSelectionChanged();
    void onDoubleClicked(const QModelIndex& index);

private slots:
    void showHeaderContextMenu(const QPoint& pos);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh parties");
    }

private:
    void setupUi();
    void setupToolbar();
    void setupTable();
    void setupColumnVisibility();
    void setupConnections();
    void updateActionStates();
    void saveSettings();
    void restoreSettings();

    ClientManager* clientManager_;
    QString username_;

    QToolBar* toolbar_;
    QTableView* tableView_;
    ClientPartyModel* model_;
    QSortFilterProxyModel* proxyModel_;

    // Toolbar actions
    QAction* reloadAction_;
    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* historyAction_;
};

}

#endif
