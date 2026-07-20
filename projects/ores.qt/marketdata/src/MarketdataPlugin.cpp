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
#include "ores.qt/MarketdataPlugin.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/CrmCrossRatesMatrixController.hpp"
#include "ores.qt/CurveSnapshotMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/FeedBindingController.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/RateCurvesMdiWindow.hpp"
#include <QAction>
#include <QMenu>

namespace ores::qt {

using namespace ores::logging;

namespace {
auto& lg() {
    static auto instance = make_logger("ores.qt.marketdata_plugin");
    return instance;
}
}

MarketdataPlugin::MarketdataPlugin(QObject* parent)
    : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

MarketdataPlugin::~MarketdataPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

void MarketdataPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    feedBindingController_ = std::make_unique<FeedBindingController>(ctx_.main_window,
                                                                     ctx_.mdi_area,
                                                                     ctx_.client_manager,
                                                                     ctx_.change_reason_cache,
                                                                     ctx_.username);

    connect(feedBindingController_.get(),
            &FeedBindingController::statusMessage,
            this,
            &PluginBase::statusMessage);
    connect(feedBindingController_.get(),
            &FeedBindingController::errorMessage,
            this,
            &PluginBase::statusMessage);
    connect(feedBindingController_.get(),
            &FeedBindingController::detachableWindowCreated,
            this,
            &PluginBase::windowCreated);
    connect(feedBindingController_.get(),
            &FeedBindingController::detachableWindowDestroyed,
            this,
            &PluginBase::windowDestroyed);

    crmCrossRatesMatrixController_ = std::make_unique<CrmCrossRatesMatrixController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache);

    connect(crmCrossRatesMatrixController_.get(),
            &CrmCrossRatesMatrixController::statusMessage,
            this,
            &PluginBase::statusMessage);
    connect(crmCrossRatesMatrixController_.get(),
            &CrmCrossRatesMatrixController::errorMessage,
            this,
            &PluginBase::statusMessage);
    connect(crmCrossRatesMatrixController_.get(),
            &CrmCrossRatesMatrixController::detachableWindowCreated,
            this,
            &PluginBase::windowCreated);
    connect(crmCrossRatesMatrixController_.get(),
            &CrmCrossRatesMatrixController::detachableWindowDestroyed,
            this,
            &PluginBase::windowDestroyed);
}

void MarketdataPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Populating shared Market Data menu.";
    marketDataMenu_ = smc.market_data_menu;
    if (!marketDataMenu_)
        return;

    using IC = IconUtils;
    auto ico = [](Icon i) {
        return IC::createRecoloredIcon(i, IC::DefaultIconColor);
    };

    marketDataMenu_->addSeparator();
    auto* actFeedBindings = marketDataMenu_->addAction(ico(Icon::ServerLink), tr("Feed &Bindings"));
    connect(actFeedBindings, &QAction::triggered, this, [this]() {
        if (feedBindingController_)
            feedBindingController_->showListWindow();
    });

    actCrmMatrix_ = marketDataMenu_->addAction(ico(Icon::Currency), tr("Cross-&Rates Matrix"));
    connect(actCrmMatrix_, &QAction::triggered, this, [this]() {
        if (crmCrossRatesMatrixController_)
            crmCrossRatesMatrixController_->showMatrix();
    });

    actRateCurves_ = marketDataMenu_->addAction(ico(Icon::Chart), tr("Interest &Rates"));
    connect(actRateCurves_, &QAction::triggered, this, &MarketdataPlugin::showRateCurves);
}

void MarketdataPlugin::showRateCurves() {
    if (rateCurvesWindow_) {
        rateCurvesWindow_->show();
        rateCurvesWindow_->raise();
        rateCurvesWindow_->activateWindow();
        return;
    }

    auto* view = new RateCurvesMdiWindow(ctx_.client_manager, ctx_.image_cache);
    connect(view, &RateCurvesMdiWindow::statusChanged, this, &PluginBase::statusMessage);
    connect(view, &RateCurvesMdiWindow::errorOccurred, this, &PluginBase::statusMessage);
    connect(view, &RateCurvesMdiWindow::viewSnapshotRequested, this,
            &MarketdataPlugin::showCurveSnapshot);

    rateCurvesWindow_ = new DetachableMdiSubWindow(ctx_.main_window);
    rateCurvesWindow_->setAttribute(Qt::WA_DeleteOnClose);
    rateCurvesWindow_->setWidget(view);
    rateCurvesWindow_->setWindowTitle(tr("Interest Rates"));
    rateCurvesWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));
    rateCurvesWindow_->resize(view->sizeHint());

    ctx_.mdi_area->addSubWindow(rateCurvesWindow_);
    rateCurvesWindow_->show();
    emit windowCreated(rateCurvesWindow_);
    connect(rateCurvesWindow_, &QObject::destroyed, this, [this]() {
        emit windowDestroyed(rateCurvesWindow_);
        rateCurvesWindow_ = nullptr;
    });
}

void MarketdataPlugin::showCurveSnapshot(const QString& seriesType, const QString& metric,
                                         const QString& qualifier) {
    auto* snapshotWindow = new CurveSnapshotMdiWindow(
        ctx_.client_manager, ctx_.image_cache, seriesType.toStdString(), metric.toStdString(),
        qualifier.toStdString());
    connect(snapshotWindow, &CurveSnapshotMdiWindow::statusChanged, this,
            &PluginBase::statusMessage);
    connect(snapshotWindow, &CurveSnapshotMdiWindow::errorOccurred, this,
            &PluginBase::statusMessage);

    auto* subWindow = new DetachableMdiSubWindow(ctx_.main_window);
    subWindow->setAttribute(Qt::WA_DeleteOnClose);
    subWindow->setWidget(snapshotWindow);
    subWindow->setWindowTitle(tr("Curve Snapshot: %1").arg(qualifier));
    subWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));
    subWindow->resize(snapshotWindow->sizeHint());

    ctx_.mdi_area->addSubWindow(subWindow);
    subWindow->show();
    emit windowCreated(subWindow);
    connect(subWindow, &QObject::destroyed, this, [this, subWindow]() {
        emit windowDestroyed(subWindow);
    });
}

QList<QMenu*> MarketdataPlugin::create_menus() {
    return {};
}

QList<QAction*> MarketdataPlugin::toolbar_actions() {
    if (!actCrmMatrix_ || !actRateCurves_)
        BOOST_LOG_SEV(lg(), warn) << "Toolbar action is uninitialised.";
    return {actCrmMatrix_, actRateCurves_};
}

void MarketdataPlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    feedBindingController_.reset();
    crmCrossRatesMatrixController_.reset();
    // rateCurvesWindow_ isn't controller-owned (RateCurvesMdiWindow has no CRUD/list-window
    // controller of its own), so nothing else closes it on logout -- close it explicitly here,
    // same as the controller-owned windows above achieve via reset(). Any open
    // CurveSnapshotMdiWindow instances are untracked (multiple can be open at once, by design,
    // to compare curves side by side) and are left as-is; their auto-refresh requests simply
    // fail gracefully post-logout like any other stale authenticated request.
    if (rateCurvesWindow_)
        rateCurvesWindow_->close();
    ctx_ = {};
}

} // namespace ores::qt
