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
#include "ores.qt/MarketDataController.hpp"

#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MarketSeriesMdiWindow.hpp"
#include "ores.qt/MarketFixingsMdiWindow.hpp"
#include "ores.qt/MarketObservationMdiWindow.hpp"
#include "ores.qt/MarketFixingDetailMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

MarketDataController::MarketDataController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, {}, parent),
      seriesListWindow_(nullptr),
      seriesListMdiSubWindow_(nullptr),
      fixingsListWindow_(nullptr),
      fixingsListMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "MarketDataController created";
}

EntityListMdiWindow* MarketDataController::listWindow() const {
    return seriesListWindow_;
}

void MarketDataController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow (market series) called";

    const QString key = build_window_key("list", "market_series");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing market series window";
        return;
    }

    seriesListWindow_ = new MarketSeriesMdiWindow(
        clientManager_, username_);

    connect(seriesListWindow_, &MarketSeriesMdiWindow::statusChanged,
            this, &MarketDataController::statusMessage);
    connect(seriesListWindow_, &MarketSeriesMdiWindow::errorOccurred,
            this, &MarketDataController::errorMessage);
    connect(seriesListWindow_, &MarketSeriesMdiWindow::showMarketObservations,
            this, &MarketDataController::onShowMarketObservations);

    seriesListMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    seriesListMdiSubWindow_->setWidget(seriesListWindow_);
    seriesListMdiSubWindow_->setWindowTitle(tr("Market Series"));
    seriesListMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ChartMultiple, IconUtils::DefaultIconColor));
    seriesListMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    seriesListMdiSubWindow_->resize(seriesListWindow_->sizeHint());

    mdiArea_->addSubWindow(seriesListMdiSubWindow_);
    seriesListMdiSubWindow_->show();

    track_window(key, seriesListMdiSubWindow_);
    register_detachable_window(seriesListMdiSubWindow_);

    connect(seriesListMdiSubWindow_, &QObject::destroyed, this,
            [self = QPointer<MarketDataController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->seriesListWindow_ = nullptr;
        self->seriesListMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Market series window created";
}

void MarketDataController::showFixingsWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showFixingsWindow called";

    const QString key = build_window_key("list", "market_fixings");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing market fixings window";
        return;
    }

    fixingsListWindow_ = new MarketFixingsMdiWindow(
        clientManager_, username_);

    connect(fixingsListWindow_, &MarketFixingsMdiWindow::statusChanged,
            this, &MarketDataController::statusMessage);
    connect(fixingsListWindow_, &MarketFixingsMdiWindow::errorOccurred,
            this, &MarketDataController::errorMessage);
    connect(fixingsListWindow_, &MarketFixingsMdiWindow::showMarketFixings,
            this, &MarketDataController::onShowMarketFixings);

    fixingsListMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    fixingsListMdiSubWindow_->setWidget(fixingsListWindow_);
    fixingsListMdiSubWindow_->setWindowTitle(tr("Market Fixings"));
    fixingsListMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, IconUtils::DefaultIconColor));
    fixingsListMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    fixingsListMdiSubWindow_->resize(fixingsListWindow_->sizeHint());

    mdiArea_->addSubWindow(fixingsListMdiSubWindow_);
    fixingsListMdiSubWindow_->show();

    track_window(key, fixingsListMdiSubWindow_);
    register_detachable_window(fixingsListMdiSubWindow_);

    connect(fixingsListMdiSubWindow_, &QObject::destroyed, this,
            [self = QPointer<MarketDataController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->fixingsListWindow_ = nullptr;
        self->fixingsListMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Market fixings window created";
}

void MarketDataController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

    const QList<QString> keys = managed_windows_.keys();
    for (const QString& key : keys) {
        if (auto* window = managed_windows_.value(key))
            window->close();
    }
    managed_windows_.clear();

    seriesListWindow_     = nullptr;
    seriesListMdiSubWindow_ = nullptr;
    fixingsListWindow_    = nullptr;
    fixingsListMdiSubWindow_ = nullptr;
}

void MarketDataController::reloadListWindow() {
    if (seriesListWindow_)
        seriesListWindow_->reload();
    if (fixingsListWindow_)
        fixingsListWindow_->reload();
}

void MarketDataController::onShowMarketObservations(
    const marketdata::domain::market_series& series) {
    BOOST_LOG_SEV(lg(), debug) << "Show observations for series: "
                               << boost::uuids::to_string(series.id);
    showObservationWindow(series);
}

void MarketDataController::onShowMarketFixings(
    const marketdata::domain::market_series& series) {
    BOOST_LOG_SEV(lg(), debug) << "Show fixings for series: "
                               << boost::uuids::to_string(series.id);
    showFixingDetailWindow(series);
}

void MarketDataController::showObservationWindow(
    const marketdata::domain::market_series& series) {
    const QString id = QString::fromStdString(boost::uuids::to_string(series.id));
    const QString key = build_window_key("observations", id);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing observation window";
        return;
    }

    auto* obsWindow = new MarketObservationMdiWindow(
        clientManager_, series);

    connect(obsWindow, &MarketObservationMdiWindow::statusChanged,
            this, &MarketDataController::statusMessage);
    connect(obsWindow, &MarketObservationMdiWindow::errorOccurred,
            this, &MarketDataController::errorMessage);

    const QString title = tr("%1 / %2 / %3")
        .arg(QString::fromStdString(series.series_type))
        .arg(QString::fromStdString(series.metric))
        .arg(QString::fromStdString(series.qualifier));

    auto* subWindow = new DetachableMdiSubWindow(mainWindow_);
    subWindow->setWidget(obsWindow);
    subWindow->setWindowTitle(title);
    subWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Histogram, IconUtils::DefaultIconColor));
    subWindow->setAttribute(Qt::WA_DeleteOnClose);

    track_window(key, subWindow);
    register_detachable_window(subWindow);

    connect(subWindow, &QObject::destroyed, this,
            [self = QPointer<MarketDataController>(this), key]() {
        if (self) self->untrack_window(key);
    });

    show_managed_window(subWindow, seriesListMdiSubWindow_);
}

void MarketDataController::showFixingDetailWindow(
    const marketdata::domain::market_series& series) {
    const QString id = QString::fromStdString(boost::uuids::to_string(series.id));
    const QString key = build_window_key("fixings", id);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing fixing detail window";
        return;
    }

    auto* fixingWindow = new MarketFixingDetailMdiWindow(
        clientManager_, series);

    connect(fixingWindow, &MarketFixingDetailMdiWindow::statusChanged,
            this, &MarketDataController::statusMessage);
    connect(fixingWindow, &MarketFixingDetailMdiWindow::errorOccurred,
            this, &MarketDataController::errorMessage);

    const QString title = tr("Fixings: %1 / %2")
        .arg(QString::fromStdString(series.metric))
        .arg(QString::fromStdString(series.qualifier));

    auto* subWindow = new DetachableMdiSubWindow(mainWindow_);
    subWindow->setWidget(fixingWindow);
    subWindow->setWindowTitle(title);
    subWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, IconUtils::DefaultIconColor));
    subWindow->setAttribute(Qt::WA_DeleteOnClose);

    track_window(key, subWindow);
    register_detachable_window(subWindow);

    connect(subWindow, &QObject::destroyed, this,
            [self = QPointer<MarketDataController>(this), key]() {
        if (self) self->untrack_window(key);
    });

    show_managed_window(subWindow, fixingsListMdiSubWindow_);
}

}
