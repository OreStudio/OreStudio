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

#include <QMenu>
#include <QAction>
#include <QMdiArea>
#include <QMdiSubWindow>
#include <QMainWindow>

#include "ores.qt/IconUtils.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/OreImportController.hpp"
#include "ores.qt/PortfolioExplorerMdiWindow.hpp"
#include "ores.qt/OrgExplorerMdiWindow.hpp"
#include "ores.qt/PortfolioController.hpp"
#include "ores.qt/BookController.hpp"
#include "ores.qt/BookStatusController.hpp"
#include "ores.qt/TradeController.hpp"

namespace ores::qt {

TradingPlugin::TradingPlugin(QObject* parent) : PluginBase(parent) {}

TradingPlugin::~TradingPlugin() = default;

// ---------------------------------------------------------------------------
// IPlugin::on_login — create all controllers
// ---------------------------------------------------------------------------
void TradingPlugin::on_login(const plugin_context& ctx) {
    ctx_ = ctx;

    oreImportController_ = std::make_unique<OreImportController>(
        ctx_.client_manager, this);
    if (!ctx_.http_base_url.empty())
        oreImportController_->setHttpBaseUrl(ctx_.http_base_url);
    connect(oreImportController_.get(), &OreImportController::statusMessage,
            this, &PluginBase::statusMessage);

    portfolioController_ = std::make_unique<PortfolioController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.badge_cache, ctx_.username, this);
    connectControllerSignals(portfolioController_.get());

    bookController_ = std::make_unique<BookController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.badge_cache, ctx_.username, this);
    connectControllerSignals(bookController_.get());

    bookStatusController_ = std::make_unique<BookStatusController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(bookStatusController_.get());

    tradeController_ = std::make_unique<TradeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(tradeController_.get());
}

// ---------------------------------------------------------------------------
// IPlugin::setup_menus — populate Trading Codes submenu and Data Transfer menu.
// ---------------------------------------------------------------------------
void TradingPlugin::setup_menus(const shared_menus_context& smc) {
    using IC = IconUtils;
    auto ico = [](Icon i) { return IC::createRecoloredIcon(i, IC::DefaultIconColor); };

    // Save reference so create_menus() can append it to the Trading menu.
    trading_codes_menu_ = smc.trading_codes_menu;

    // ---- Trading Codes — add Book Statuses (RefdataPlugin adds Purpose Types) --
    if (trading_codes_menu_) {
        auto* actBookStatuses = trading_codes_menu_->addAction(
            ico(Icon::Flag), tr("Book &Statuses"));
        connect(actBookStatuses, &QAction::triggered, this, [this]() {
            if (bookStatusController_) bookStatusController_->showListWindow();
        });
    }

    // ---- Data Transfer menu — contribute Import ORE Data ----------------
    if (smc.data_transfer_menu) {
        auto* actImportOre = smc.data_transfer_menu->addAction(
            ico(Icon::ImportOre), tr("&Import ORE Data..."));
        connect(actImportOre, &QAction::triggered, this, [this]() {
            if (oreImportController_) oreImportController_->trigger(ctx_.main_window);
        });
    }
}

// ---------------------------------------------------------------------------
// IPlugin::create_menus — return the Trading menu (portfolios, books, explorers,
// trades).  The standalone Data menu was removed; portfolios and books now live
// at the top of the Trading menu.
// ---------------------------------------------------------------------------
QList<QMenu*> TradingPlugin::create_menus() {
    using IC = IconUtils;
    auto ico = [](Icon i) { return IC::createRecoloredIcon(i, IC::DefaultIconColor); };

    // ---- Trading ----------------------------------------------------
    auto* menuTrading = new QMenu(tr("&Trading"));

    act_portfolios_ = menuTrading->addAction(ico(Icon::Briefcase), tr("&Portfolios"));
    connect(act_portfolios_, &QAction::triggered, this, [this]() {
        if (portfolioController_) portfolioController_->showListWindow();
    });
    act_books_ = menuTrading->addAction(ico(Icon::BookOpen), tr("&Books"));
    connect(act_books_, &QAction::triggered, this, [this]() {
        if (bookController_) bookController_->showListWindow();
    });

    menuTrading->addSeparator();

    act_portfolio_explorer_ = menuTrading->addAction(
        ico(Icon::BriefcaseFilled), tr("&Portfolio Explorer"));
    connect(act_portfolio_explorer_, &QAction::triggered, this, [this]() {
        if (portfolio_explorer_sub_window_) {
            ctx_.mdi_area->setActiveSubWindow(portfolio_explorer_sub_window_);
            return;
        }

        auto* window = new PortfolioExplorerMdiWindow(
            ctx_.client_manager,
            bookController_.get(),
            portfolioController_.get(),
            tradeController_.get(),
            oreImportController_.get(),
            ctx_.username,
            ctx_.main_window);

        connect(window, &PortfolioExplorerMdiWindow::statusChanged,
                this, [this](const QString& msg) { emit statusMessage(msg); });

        auto* subWindow = new DetachableMdiSubWindow(ctx_.main_window);
        subWindow->setWidget(window);
        subWindow->setWindowTitle(tr("Portfolio Explorer"));
        subWindow->setWindowIcon(
            IconUtils::createRecoloredIcon(Icon::BriefcaseFilled, IconUtils::DefaultIconColor));
        subWindow->setAttribute(Qt::WA_DeleteOnClose);

        portfolio_explorer_sub_window_ = subWindow;
        connect(subWindow, &QObject::destroyed, this, [this]() {
            portfolio_explorer_sub_window_ = nullptr;
        });

        ctx_.mdi_area->addSubWindow(subWindow);
        subWindow->resize(window->sizeHint());
        subWindow->show();
    });

    act_org_explorer_ = menuTrading->addAction(ico(Icon::Organization), tr("&Org Explorer"));
    connect(act_org_explorer_, &QAction::triggered, this, [this]() {
        if (org_explorer_sub_window_) {
            ctx_.mdi_area->setActiveSubWindow(org_explorer_sub_window_);
            return;
        }

        auto* window = new OrgExplorerMdiWindow(
            ctx_.client_manager,
            nullptr,
            bookController_.get(),
            tradeController_.get(),
            ctx_.username,
            ctx_.main_window);

        connect(window, &OrgExplorerMdiWindow::statusChanged,
                this, [this](const QString& msg) { emit statusMessage(msg); });

        auto* subWindow = new DetachableMdiSubWindow(ctx_.main_window);
        subWindow->setWidget(window);
        subWindow->setWindowTitle(tr("Org Explorer"));
        subWindow->setWindowIcon(
            IconUtils::createRecoloredIcon(Icon::Organization, IconUtils::DefaultIconColor));
        subWindow->setAttribute(Qt::WA_DeleteOnClose);

        org_explorer_sub_window_ = subWindow;
        connect(subWindow, &QObject::destroyed, this, [this]() {
            org_explorer_sub_window_ = nullptr;
        });

        ctx_.mdi_area->addSubWindow(subWindow);
        subWindow->resize(window->sizeHint());
        subWindow->show();
    });

    menuTrading->addSeparator();
    act_trades_ = menuTrading->addAction(ico(Icon::ArrowTrending), tr("&Trades"));
    connect(act_trades_, &QAction::triggered, this, [this]() {
        if (tradeController_) tradeController_->showListWindow();
    });

    // Append the Trading Codes submenu (populated during setup_menus).
    if (trading_codes_menu_) {
        menuTrading->addSeparator();
        menuTrading->addMenu(trading_codes_menu_);
    }

    return {menuTrading};
}

QList<QAction*> TradingPlugin::toolbar_actions() {
    return {act_portfolios_, act_books_, act_portfolio_explorer_,
            act_org_explorer_, act_trades_};
}

// ---------------------------------------------------------------------------
// IPlugin::on_logout — close open singleton windows, destroy all controllers
// ---------------------------------------------------------------------------
void TradingPlugin::on_logout() {
    if (portfolio_explorer_sub_window_) {
        portfolio_explorer_sub_window_->close();
        portfolio_explorer_sub_window_ = nullptr;
    }
    if (org_explorer_sub_window_) {
        org_explorer_sub_window_->close();
        org_explorer_sub_window_ = nullptr;
    }

    tradeController_.reset();
    bookStatusController_.reset();
    bookController_.reset();
    portfolioController_.reset();
    oreImportController_.reset();

    ctx_ = {};
}

} // namespace ores::qt
