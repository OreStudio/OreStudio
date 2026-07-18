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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.qt/TradingPlugin.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/BookController.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IBusinessUnitBrowser.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/OreImportController.hpp"
#include "ores.qt/OrgExplorerMdiWindow.hpp"
#include "ores.qt/PluginRegistry.hpp"
#include "ores.qt/PortfolioController.hpp"
#include "ores.qt/PortfolioExplorerMdiWindow.hpp"
#include "ores.qt/RefdataPlugin.hpp"
#include "ores.qt/TradeController.hpp"
#include <QAction>
#include <QMainWindow>
#include <QMdiArea>
#include <QMdiSubWindow>
#include <QMenu>

namespace ores::qt {

using namespace ores::logging;

namespace {
auto& lg() {
    static auto instance = make_logger("ores.qt.trading_plugin");
    return instance;
}

// RefdataPlugin (load_order 100) always runs on_login() before TradingPlugin
// (load_order 200), so its controllers are constructed by the time this
// runs; see PluginRegistry::plugins(), sorted ascending by load_order.
BookController* find_book_controller() {
    for (auto* plugin : PluginRegistry::instance().plugins()) {
        if (auto* refdata = dynamic_cast<RefdataPlugin*>(plugin))
            return refdata->book_controller();
    }
    return nullptr;
}

IBusinessUnitBrowser* find_business_unit_controller() {
    for (auto* plugin : PluginRegistry::instance().plugins()) {
        if (auto* refdata = dynamic_cast<RefdataPlugin*>(plugin))
            return refdata->business_unit_controller();
    }
    return nullptr;
}

PortfolioController* find_portfolio_controller() {
    for (auto* plugin : PluginRegistry::instance().plugins()) {
        if (auto* refdata = dynamic_cast<RefdataPlugin*>(plugin))
            return refdata->portfolio_controller();
    }
    return nullptr;
}
}

TradingPlugin::TradingPlugin(QObject* parent)
    : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

TradingPlugin::~TradingPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

// ---------------------------------------------------------------------------
// IPlugin::on_login — create all controllers
// ---------------------------------------------------------------------------
void TradingPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    oreImportController_ = std::make_unique<OreImportController>(ctx_.client_manager, this);
    if (!ctx_.http_base_url.empty())
        oreImportController_->setHttpBaseUrl(ctx_.http_base_url);
    connect(oreImportController_.get(),
            &OreImportController::statusMessage,
            this,
            &PluginBase::statusMessage);

    // Book and Portfolio are owned by RefdataPlugin -- no cross-component
    // leakage. Resolve non-owning pointers for the composite Explorer
    // views below.
    bookController_ = find_book_controller();
    if (bookController_)
        connectControllerSignals(bookController_);
    else
        BOOST_LOG_SEV(lg(), warn) << "RefdataPlugin's BookController not found -- "
                                  << "Explorer views will be non-functional.";

    portfolioController_ = find_portfolio_controller();
    if (portfolioController_)
        connectControllerSignals(portfolioController_);
    else
        BOOST_LOG_SEV(lg(), warn) << "RefdataPlugin's PortfolioController not found -- "
                                  << "Explorer views will be non-functional.";

    businessUnitController_ = find_business_unit_controller();
    if (!businessUnitController_)
        BOOST_LOG_SEV(lg(), warn)
            << "RefdataPlugin's BusinessUnitController not found -- "
            << "Org Explorer's business unit edit/history will be non-functional.";

    tradeController_ = std::make_unique<TradeController>(ctx_.main_window,
                                                         ctx_.mdi_area,
                                                         ctx_.client_manager,
                                                         ctx_.change_reason_cache,
                                                         ctx_.image_cache,
                                                         ctx_.username,
                                                         this);
    connectControllerSignals(tradeController_.get());
}

// ---------------------------------------------------------------------------
// IPlugin::setup_menus — populate the Data Transfer menu.
// ---------------------------------------------------------------------------
void TradingPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Registering entries in shared menus."
                               << " data_transfer=" << (smc.data_transfer_menu ? "ok" : "null");
    using IC = IconUtils;
    auto ico = [](Icon i) {
        return IC::createRecoloredIcon(i, IC::DefaultIconColor);
    };

    // ---- Data Transfer menu — contribute Import ORE Data -----------------
    if (smc.data_transfer_menu) {
        smc.data_transfer_menu->addSeparator();
        auto* actImportOre =
            smc.data_transfer_menu->addAction(ico(Icon::ImportOre), tr("&Import ORE Data..."));
        connect(actImportOre, &QAction::triggered, this, [this]() {
            if (oreImportController_)
                oreImportController_->trigger(ctx_.main_window);
        });
    }
}

// ---------------------------------------------------------------------------
// IPlugin::create_menus — return the Trading menu (portfolios, books, explorers,
// trades).  The standalone Data menu was removed; portfolios and books now live
// at the top of the Trading menu.
// ---------------------------------------------------------------------------
QList<QMenu*> TradingPlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "Building plugin menus.";
    using IC = IconUtils;
    auto ico = [](Icon i) {
        return IC::createRecoloredIcon(i, IC::DefaultIconColor);
    };

    // ---- Trading ----------------------------------------------------
    auto* menuTrading = new QMenu(tr("&Trading"));

    act_portfolio_explorer_ =
        menuTrading->addAction(ico(Icon::BriefcaseFilled), tr("&Portfolio Explorer"));
    connect(act_portfolio_explorer_, &QAction::triggered, this, [this]() {
        if (portfolio_explorer_sub_window_) {
            ctx_.mdi_area->setActiveSubWindow(portfolio_explorer_sub_window_);
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "DIAG: creating PortfolioExplorerMdiWindow";
        auto* window = new PortfolioExplorerMdiWindow(ctx_.client_manager,
                                                      bookController_,
                                                      portfolioController_,
                                                      tradeController_.get(),
                                                      oreImportController_.get(),
                                                      ctx_.username,
                                                      ctx_.main_window);
        BOOST_LOG_SEV(lg(), info) << "DIAG: PortfolioExplorerMdiWindow constructor returned";

        connect(window,
                &PortfolioExplorerMdiWindow::statusChanged,
                this,
                [this](const QString& msg) { emit statusMessage(msg); });

        BOOST_LOG_SEV(lg(), info) << "DIAG: new DetachableMdiSubWindow";
        auto* subWindow = new DetachableMdiSubWindow(ctx_.main_window);
        BOOST_LOG_SEV(lg(), info) << "DIAG: setWidget";
        subWindow->setWidget(window);
        BOOST_LOG_SEV(lg(), info) << "DIAG: setWindowTitle";
        subWindow->setWindowTitle(tr("Portfolio Explorer"));
        BOOST_LOG_SEV(lg(), info) << "DIAG: setWindowIcon";
        subWindow->setWindowIcon(
            IconUtils::createRecoloredIcon(Icon::BriefcaseFilled, IconUtils::DefaultIconColor));
        BOOST_LOG_SEV(lg(), info) << "DIAG: setAttribute";
        subWindow->setAttribute(Qt::WA_DeleteOnClose);

        portfolio_explorer_sub_window_ = subWindow;
        connect(subWindow, &QObject::destroyed, this, [this]() {
            portfolio_explorer_sub_window_ = nullptr;
        });

        BOOST_LOG_SEV(lg(), info) << "DIAG: calling addSubWindow";
        ctx_.mdi_area->addSubWindow(subWindow);
        BOOST_LOG_SEV(lg(), info) << "DIAG: addSubWindow returned, calling sizeHint";
        const auto hint = window->sizeHint();
        BOOST_LOG_SEV(lg(), info) << "DIAG: sizeHint returned " << hint.width() << "x"
                                  << hint.height() << ", calling resize";
        subWindow->resize(hint);
        BOOST_LOG_SEV(lg(), info) << "DIAG: resize returned, calling show";
        subWindow->show();
        BOOST_LOG_SEV(lg(), info) << "DIAG: subWindow->show() returned, action handler done";
    });

    act_org_explorer_ = menuTrading->addAction(ico(Icon::Organization), tr("&Org Explorer"));
    connect(act_org_explorer_, &QAction::triggered, this, [this]() {
        if (org_explorer_sub_window_) {
            ctx_.mdi_area->setActiveSubWindow(org_explorer_sub_window_);
            return;
        }

        auto* window = new OrgExplorerMdiWindow(ctx_.client_manager,
                                                businessUnitController_,
                                                bookController_,
                                                tradeController_.get(),
                                                ctx_.username,
                                                ctx_.main_window);

        connect(window, &OrgExplorerMdiWindow::statusChanged, this, [this](const QString& msg) {
            emit statusMessage(msg);
        });

        auto* subWindow = new DetachableMdiSubWindow(ctx_.main_window);
        subWindow->setWidget(window);
        subWindow->setWindowTitle(tr("Org Explorer"));
        subWindow->setWindowIcon(
            IconUtils::createRecoloredIcon(Icon::Organization, IconUtils::DefaultIconColor));
        subWindow->setAttribute(Qt::WA_DeleteOnClose);

        org_explorer_sub_window_ = subWindow;
        connect(
            subWindow, &QObject::destroyed, this, [this]() { org_explorer_sub_window_ = nullptr; });

        ctx_.mdi_area->addSubWindow(subWindow);
        subWindow->resize(window->sizeHint());
        subWindow->show();
    });

    menuTrading->addSeparator();
    act_trades_ = menuTrading->addAction(ico(Icon::ArrowTrending), tr("&Trades"));
    connect(act_trades_, &QAction::triggered, this, [this]() {
        if (tradeController_)
            tradeController_->showListWindow();
    });

    BOOST_LOG_SEV(lg(), debug) << "Plugin menus ready.";
    return {menuTrading};
}

QList<QAction*> TradingPlugin::toolbar_actions() {
    if (!act_portfolio_explorer_ || !act_org_explorer_ || !act_trades_)
        BOOST_LOG_SEV(lg(), warn) << "One or more toolbar actions are uninitialised.";
    return {act_portfolio_explorer_, act_org_explorer_, act_trades_};
}

// ---------------------------------------------------------------------------
// IPlugin::on_logout — close open singleton windows, destroy all controllers
// ---------------------------------------------------------------------------
void TradingPlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    if (portfolio_explorer_sub_window_) {
        portfolio_explorer_sub_window_->close();
        portfolio_explorer_sub_window_ = nullptr;
    }
    if (org_explorer_sub_window_) {
        org_explorer_sub_window_->close();
        org_explorer_sub_window_ = nullptr;
    }

    tradeController_.reset();
    bookController_ = nullptr;         // non-owning; RefdataPlugin destroys the real object
    businessUnitController_ = nullptr; // non-owning; RefdataPlugin destroys the real object
    portfolioController_ = nullptr;    // non-owning; RefdataPlugin destroys the real object
    oreImportController_.reset();

    ctx_ = {};
}

} // namespace ores::qt
