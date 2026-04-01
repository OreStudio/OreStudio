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
#ifndef ORES_QT_MARKET_FIXING_DETAIL_MDI_WINDOW_HPP
#define ORES_QT_MARKET_FIXING_DETAIL_MDI_WINDOW_HPP

#include <memory>
#include <QLabel>
#include <QTableView>
#include <QToolBar>
#include <QVBoxLayout>
#include <QSortFilterProxyModel>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientMarketFixingModel.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"

namespace ores::qt {

/**
 * @brief MDI window showing the fixing history for a single index series.
 */
class MarketFixingDetailMdiWindow final : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.market_fixing_detail_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit MarketFixingDetailMdiWindow(
        ClientManager* clientManager,
        const marketdata::domain::market_series& series,
        QWidget* parent = nullptr);
    ~MarketFixingDetailMdiWindow() override = default;

    [[nodiscard]] const boost::uuids::uuid& seriesId() const;

public slots:
    void doReload() override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh fixings");
    }

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});

private:
    void setupUi();
    void setupToolbar();

    marketdata::domain::market_series series_;
    QVBoxLayout* verticalLayout_;
    QLabel* seriesInfoLabel_;
    QTableView* tableView_;
    QToolBar* toolBar_;
    QAction* reloadAction_;

    std::unique_ptr<ClientMarketFixingModel> model_;
    QSortFilterProxyModel* proxyModel_;
    ClientManager* clientManager_;
};

}

#endif
