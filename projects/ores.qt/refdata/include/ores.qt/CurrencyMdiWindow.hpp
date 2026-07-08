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
#ifndef ORES_QT_CURRENCY_MDI_WINDOW_HPP
#define ORES_QT_CURRENCY_MDI_WINDOW_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/xml/importer.hpp"
#include "ores.qt/ClientCurrencyModel.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/PaginationWidget.hpp"
#include "ores.refdata.api/csv/exporter.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include <QSortFilterProxyModel>
#include <QTableView>
#include <QToolBar>

namespace ores::qt {

class ImageCache;

/**
 * @brief MDI window for displaying and managing currencies.
 *
 * Provides a table view of currencies with toolbar actions
 * for reload, add, edit, delete, and viewing history.
 */
class CurrencyMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.currency_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CurrencyMdiWindow(ClientManager* clientManager,
                               const QString& username,
                               ImageCache* imageCache,
                               QWidget* parent = nullptr);
    ~CurrencyMdiWindow() override = default;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void showCurrencyDetails(const refdata::domain::currency& currency);
    void addNewRequested();
    void currencyDeleted(const QString& code);
    void showCurrencyHistory(const refdata::domain::currency& currency);

    /**
     * @brief Emitted to request opening a related entity's own list window
     * (e.g. a lookup entity backing one of this entity's combo fields).
     * Relayed by the controller and wired to the target's controller in the
     * plugin's composition root.
     */
    void showRoundingTypesRequested();
    void showMonetaryNaturesRequested();
    void showMarketTiersRequested();

public slots:
    void addNew();
    void editSelected();
    void deleteSelected();
    void viewHistorySelected();
    void exportToCSV();
    void exportToXML();
    void importFromXML();

protected:
    void doReload() override;

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onSelectionChanged();
    void onDoubleClicked(const QModelIndex& index);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh currencies");
    }

private:
    void setupUi();
    void setupToolbar();
    void setupTable();
    void setupConnections();
    void updateActionStates();

    ClientManager* clientManager_;
    QString username_;
    ImageCache* imageCache_;

    QToolBar* toolbar_;
    QTableView* tableView_;
    ClientCurrencyModel* model_;
    QSortFilterProxyModel* proxyModel_;
    PaginationWidget* paginationWidget_;

    // Toolbar actions
    QAction* reloadAction_;
    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* importXMLAction_;
    QAction* exportCSVAction_;
    QAction* exportXMLAction_;
    QAction* historyAction_;
};

}

#endif
