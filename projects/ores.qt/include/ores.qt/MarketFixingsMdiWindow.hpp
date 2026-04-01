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
#ifndef ORES_QT_MARKET_FIXINGS_MDI_WINDOW_HPP
#define ORES_QT_MARKET_FIXINGS_MDI_WINDOW_HPP

#include <memory>
#include <QTableView>
#include <QToolBar>
#include <QVBoxLayout>
#include <QSortFilterProxyModel>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientMarketSeriesModel.hpp"
#include "ores.qt/PaginationWidget.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"

namespace ores::qt {

/**
 * @brief MDI window listing index fixing series and their historical fixing values.
 *
 * Shows the catalog of index series (series_type = "FIXING"). Double-clicking
 * or pressing "View Fixings" opens the fixing history window for the selected
 * index.
 */
class MarketFixingsMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.market_fixings_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit MarketFixingsMdiWindow(ClientManager* clientManager,
                                    const QString& username,
                                    QWidget* parent = nullptr);
    ~MarketFixingsMdiWindow() override = default;

    ClientMarketSeriesModel* seriesModel() const { return model_.get(); }

public slots:
    void doReload() override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void showMarketFixings(const marketdata::domain::market_series& series);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh fixing series");
    }

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onRowDoubleClicked(const QModelIndex& index);
    void onSelectionChanged();
    void onConnectionStateChanged();
    void viewFixings();

private:
    void setupUi();
    void setupToolbar();
    void updateActionStates();

    QVBoxLayout* verticalLayout_;
    QTableView* tableView_;
    QToolBar* toolBar_;
    PaginationWidget* paginationWidget_;

    QAction* reloadAction_;
    QAction* viewFixingsAction_;

    std::unique_ptr<ClientMarketSeriesModel> model_;
    QSortFilterProxyModel* proxyModel_;
    ClientManager* clientManager_;
    QString username_;
};

}

#endif
