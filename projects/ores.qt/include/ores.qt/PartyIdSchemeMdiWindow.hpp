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
#ifndef ORES_QT_PARTY_ID_SCHEME_MDI_WINDOW_HPP
#define ORES_QT_PARTY_ID_SCHEME_MDI_WINDOW_HPP

#include <QToolBar>
#include <QTableView>
#include <QSortFilterProxyModel>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientPartyIdSchemeModel.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/party_id_scheme.hpp"

namespace ores::qt {

/**
 * @brief MDI window for displaying and managing party ID schemes.
 *
 * Provides a table view of party ID schemes with toolbar actions
 * for reload, add, edit, delete, and viewing history.
 */
class PartyIdSchemeMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.party_id_scheme_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PartyIdSchemeMdiWindow(
        ClientManager* clientManager,
        const QString& username,
        QWidget* parent = nullptr);
    ~PartyIdSchemeMdiWindow() override = default;

    QSize sizeHint() const override;

public slots:
    void reload() override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void showSchemeDetails(const refdata::domain::party_id_scheme& scheme);
    void addNewRequested();
    void schemeDeleted(const QString& code);
    void showSchemeHistory(const refdata::domain::party_id_scheme& scheme);

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
        return tr("Refresh party ID schemes");
    }

private:
    void setupUi();
    void setupToolbar();
    void setupTable();
    void setupColumnVisibility();
    void setupConnections();
    void updateActionStates();
    void saveSettings() override;
    void restoreSettings();

    ClientManager* clientManager_;
    QString username_;

    QToolBar* toolbar_;
    QTableView* tableView_;
    ClientPartyIdSchemeModel* model_;
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
