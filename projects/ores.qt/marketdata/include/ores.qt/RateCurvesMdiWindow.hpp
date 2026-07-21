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
#ifndef ORES_QT_RATE_CURVES_MDI_WINDOW_HPP
#define ORES_QT_RATE_CURVES_MDI_WINDOW_HPP

#include "ores.logging/make_logger.hpp"
#include <QWidget>
#include <string>
#include <vector>

class QToolBar;
class QAction;
class QTableView;
class QStandardItemModel;
class QSortFilterProxyModel;
class QLabel;
class QModelIndex;

namespace ores::qt {

class ClientManager;
class ImageCache;

/**
 * @brief Lists the official market_series catalog entries for asset_class::rates -- the
 * entry point for interest-rate curve analysis, independent of how each series is produced
 * (=ores.synthetic= today, a real vendor feed like Bloomberg tomorrow). Selecting a row opens
 * a CurveSnapshotMdiWindow for that series.
 *
 * The Qualifier column shows a currency flag: for a RATES/YIELD series, market_series.qualifier
 * is documented (see ir_curve_tick::qualifier) as "currency/index" -- the third+ segment of the
 * series' ORE key, the same segment structure feed_binding.ore_key uses -- so its first segment
 * is currency by producer contract, not a guessed convention.
 */
class RateCurvesMdiWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.marketdata.rate_curves_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    RateCurvesMdiWindow(ClientManager* clientManager,
                        ImageCache* imageCache,
                        QWidget* parent = nullptr);
    ~RateCurvesMdiWindow() override = default;

    QSize sizeHint() const override {
        return {900, 500};
    }

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& message);
    void viewSnapshotRequested(const QString& seriesType,
                               const QString& metric,
                               const QString& qualifier);

private slots:
    void reload();
    void onRowActivated(const QModelIndex& index);

private:
    void setupUi();

    struct Row {
        std::string series_type;
        std::string metric;
        std::string qualifier;
        std::string series_subclass;
    };

    ClientManager* clientManager_;
    ImageCache* imageCache_;
    QToolBar* toolbar_;
    QAction* reloadAction_;
    QTableView* tableView_;
    QStandardItemModel* model_;
    QSortFilterProxyModel* proxyModel_;
    QLabel* emptyLabel_;
    std::vector<Row> rows_;
};

}

#endif
