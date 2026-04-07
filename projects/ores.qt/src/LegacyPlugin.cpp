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
#include "ores.qt/LegacyPlugin.hpp"

#include <QMenu>
#include <QAction>
#include <QMdiArea>
#include <QMdiSubWindow>
#include <QMainWindow>

#include "ores.qt/IconUtils.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/PortfolioExplorerMdiWindow.hpp"
#include "ores.qt/OrgExplorerMdiWindow.hpp"
#include "ores.qt/DataLibrarianWindow.hpp"
#include "ores.qt/PartyTypeController.hpp"
#include "ores.qt/PartyStatusController.hpp"
#include "ores.qt/PartyIdSchemeController.hpp"
#include "ores.qt/ContactTypeController.hpp"
#include "ores.qt/PartyController.hpp"
#include "ores.qt/CounterpartyController.hpp"
#include "ores.qt/BusinessCentreController.hpp"
#include "ores.qt/BusinessUnitController.hpp"
#include "ores.qt/BusinessUnitTypeController.hpp"
#include "ores.qt/PortfolioController.hpp"
#include "ores.qt/BookController.hpp"
#include "ores.qt/BookStatusController.hpp"
#include "ores.qt/CurrencyMarketTierController.hpp"
#include "ores.qt/TradeController.hpp"
#include "ores.qt/PricingEngineTypeController.hpp"
#include "ores.qt/PricingModelConfigController.hpp"
#include "ores.qt/PricingModelProductController.hpp"
#include "ores.qt/PricingModelProductParameterController.hpp"
#include "ores.qt/MarketDataController.hpp"

namespace ores::qt {

LegacyPlugin::LegacyPlugin(QObject* parent) : QObject(parent) {}

LegacyPlugin::~LegacyPlugin() = default;

// ---------------------------------------------------------------------------
// Helper: wire standard controller signals to LegacyPlugin forwarding slots.
// Controllers that lack detachableWindow* signals are connected individually.
// ---------------------------------------------------------------------------
void LegacyPlugin::connect_controller_signals(QObject* ctrl) {
    connect(ctrl, SIGNAL(statusMessage(const QString&)),
            this, SLOT(on_status_message(const QString&)));
    connect(ctrl, SIGNAL(errorMessage(const QString&)),
            this, SLOT(on_status_message(const QString&)));
    connect(ctrl, SIGNAL(detachableWindowCreated(DetachableMdiSubWindow*)),
            this, SLOT(on_window_created(DetachableMdiSubWindow*)));
    connect(ctrl, SIGNAL(detachableWindowDestroyed(DetachableMdiSubWindow*)),
            this, SLOT(on_window_destroyed(DetachableMdiSubWindow*)));
}

// ---------------------------------------------------------------------------
// IPlugin::on_login — create all controllers and wire inter-controller relays
// ---------------------------------------------------------------------------
void LegacyPlugin::on_login(const plugin_context& ctx) {
    ctx_ = ctx;

    partyTypeController_ = std::make_unique<PartyTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(partyTypeController_.get());

    partyStatusController_ = std::make_unique<PartyStatusController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(partyStatusController_.get());

    partyIdSchemeController_ = std::make_unique<PartyIdSchemeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(partyIdSchemeController_.get());

    contactTypeController_ = std::make_unique<ContactTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(contactTypeController_.get());

    partyController_ = std::make_unique<PartyController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.badge_cache, ctx_.username, this);
    connect_controller_signals(partyController_.get());

    counterpartyController_ = std::make_unique<CounterpartyController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.badge_cache, ctx_.username, this);
    connect_controller_signals(counterpartyController_.get());

    businessCentreController_ = std::make_unique<BusinessCentreController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(businessCentreController_.get());

    businessUnitController_ = std::make_unique<BusinessUnitController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.badge_cache, ctx_.username, this);
    connect_controller_signals(businessUnitController_.get());

    businessUnitTypeController_ = std::make_unique<BusinessUnitTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(businessUnitTypeController_.get());

    portfolioController_ = std::make_unique<PortfolioController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.badge_cache, ctx_.username, this);
    connect_controller_signals(portfolioController_.get());

    bookController_ = std::make_unique<BookController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.badge_cache, ctx_.username, this);
    connect_controller_signals(bookController_.get());

    bookStatusController_ = std::make_unique<BookStatusController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(bookStatusController_.get());

    currencyMarketTierController_ = std::make_unique<CurrencyMarketTierController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(currencyMarketTierController_.get());

    tradeController_ = std::make_unique<TradeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connect_controller_signals(tradeController_.get());

    pricingEngineTypeController_ = std::make_unique<PricingEngineTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connect_controller_signals(pricingEngineTypeController_.get());

    pricingModelConfigController_ = std::make_unique<PricingModelConfigController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connect_controller_signals(pricingModelConfigController_.get());

    pricingModelProductController_ = std::make_unique<PricingModelProductController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connect_controller_signals(pricingModelProductController_.get());

    pricingModelProductParameterController_ = std::make_unique<PricingModelProductParameterController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connect_controller_signals(pricingModelProductParameterController_.get());

    marketDataController_ = std::make_unique<MarketDataController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username);
    connect(marketDataController_.get(), &MarketDataController::statusMessage,
            this, &LegacyPlugin::status_message);
    connect(marketDataController_.get(), &MarketDataController::errorMessage,
            this, &LegacyPlugin::status_message);
    connect(marketDataController_.get(), &MarketDataController::detachableWindowCreated,
            this, &LegacyPlugin::window_created);
    connect(marketDataController_.get(), &MarketDataController::detachableWindowDestroyed,
            this, &LegacyPlugin::window_destroyed);
}

// ---------------------------------------------------------------------------
// IPlugin::create_menus — build domain menus from code and return them.
// The host inserts these into the menu bar after on_login().
// ---------------------------------------------------------------------------
QList<QMenu*> LegacyPlugin::create_menus() {
    using IC = IconUtils;
    auto ico = [](Icon i) { return IC::createRecoloredIcon(i, IC::DefaultIconColor); };

    // ---- Data -------------------------------------------------------
    auto* menuData = new QMenu(tr("&Data"));

    auto* actBusinessCentres = menuData->addAction(
        ico(Icon::BuildingBank), tr("&Business Centres"));
    connect(actBusinessCentres, &QAction::triggered, this, [this]() {
        if (businessCentreController_) businessCentreController_->showListWindow();
    });
    menuData->addSeparator();
    auto* actParties = menuData->addAction(ico(Icon::Organization), tr("&Parties"));
    connect(actParties, &QAction::triggered, this, [this]() {
        if (partyController_) partyController_->showListWindow();
    });
    auto* actCounterparties = menuData->addAction(ico(Icon::Handshake), tr("&Counterparties"));
    connect(actCounterparties, &QAction::triggered, this, [this]() {
        if (counterpartyController_) counterpartyController_->showListWindow();
    });
    menuData->addSeparator();
    auto* actBizUnits = menuData->addAction(ico(Icon::PeopleTeam), tr("Business &Units"));
    connect(actBizUnits, &QAction::triggered, this, [this]() {
        if (businessUnitController_) businessUnitController_->showListWindow();
    });
    auto* actBizUnitTypes = menuData->addAction(ico(Icon::PeopleTeam), tr("Business Unit &Types"));
    connect(actBizUnitTypes, &QAction::triggered, this, [this]() {
        if (businessUnitTypeController_) businessUnitTypeController_->showListWindow();
    });
    auto* actPortfolios = menuData->addAction(ico(Icon::Briefcase), tr("&Portfolios"));
    connect(actPortfolios, &QAction::triggered, this, [this]() {
        if (portfolioController_) portfolioController_->showListWindow();
    });
    auto* actBooks = menuData->addAction(ico(Icon::BookOpen), tr("&Books"));
    connect(actBooks, &QAction::triggered, this, [this]() {
        if (bookController_) bookController_->showListWindow();
    });
    menuData->addSeparator();

    // Auxiliary Data submenu
    auto* menuAux = menuData->addMenu(tr("A&uxiliary Data"));
    auto* actCurrencyMarketTiers = menuAux->addAction(ico(Icon::Chart), tr("Currency Market &Tiers"));
    connect(actCurrencyMarketTiers, &QAction::triggered, this, [this]() {
        if (currencyMarketTierController_) currencyMarketTierController_->showListWindow();
    });
    menuAux->addSeparator();
    auto* actPartyTypes = menuAux->addAction(ico(Icon::Tag), tr("Party &Types"));
    connect(actPartyTypes, &QAction::triggered, this, [this]() {
        if (partyTypeController_) partyTypeController_->showListWindow();
    });
    auto* actPartyStatuses = menuAux->addAction(ico(Icon::Flag), tr("Party &Statuses"));
    connect(actPartyStatuses, &QAction::triggered, this, [this]() {
        if (partyStatusController_) partyStatusController_->showListWindow();
    });
    auto* actPartyIdSchemes = menuAux->addAction(ico(Icon::Key), tr("Party &ID Schemes"));
    connect(actPartyIdSchemes, &QAction::triggered, this, [this]() {
        if (partyIdSchemeController_) partyIdSchemeController_->showListWindow();
    });
    auto* actContactTypes = menuAux->addAction(ico(Icon::PersonAccounts), tr("&Contact Types"));
    connect(actContactTypes, &QAction::triggered, this, [this]() {
        if (contactTypeController_) contactTypeController_->showListWindow();
    });
    menuAux->addSeparator();
    auto* actBookStatuses = menuAux->addAction(ico(Icon::Flag), tr("Book &Statuses"));
    connect(actBookStatuses, &QAction::triggered, this, [this]() {
        if (bookStatusController_) bookStatusController_->showListWindow();
    });

    // Assets submenu (DataLibrarianWindow moves to mktdata in Step 7)
    auto* menuAssets = menuData->addMenu(tr("&Assets"));
    auto* actDataLibrarian = menuAssets->addAction(ico(Icon::Library), tr("Data &Librarian"));
    connect(actDataLibrarian, &QAction::triggered, this, [this]() {
        if (data_librarian_window_) {
            ctx_.mdi_area->setActiveSubWindow(
                qobject_cast<QMdiSubWindow*>(data_librarian_window_->parent()));
            return;
        }

        auto* librarianWindow = new DataLibrarianWindow(
            ctx_.client_manager, ctx_.username, ctx_.badge_cache, ctx_.main_window);

        auto* subWindow = new DetachableMdiSubWindow(ctx_.main_window);
        subWindow->setWidget(librarianWindow);
        subWindow->setWindowTitle(tr("Data Librarian"));
        subWindow->setWindowIcon(IconUtils::createRecoloredIcon(
            Icon::Library, IconUtils::DefaultIconColor));
        subWindow->setAttribute(Qt::WA_DeleteOnClose);

        connect(librarianWindow, &DataLibrarianWindow::statusChanged,
                this, [this](const QString& msg) { emit status_message(msg); });
        connect(librarianWindow, &DataLibrarianWindow::errorOccurred,
                this, [this](const QString& msg) { emit status_message(msg); });

        data_librarian_window_ = subWindow;
        connect(subWindow, &QObject::destroyed, this, [this]() {
            data_librarian_window_ = nullptr;
        });

        ctx_.mdi_area->addSubWindow(subWindow);
        subWindow->resize(librarianWindow->sizeHint());
        subWindow->show();
    });

    // ---- Trading ----------------------------------------------------
    auto* menuTrading = new QMenu(tr("&Trading"));

    auto* actPortfolioExplorer = menuTrading->addAction(
        ico(Icon::BriefcaseFilled), tr("&Portfolio Explorer"));
    connect(actPortfolioExplorer, &QAction::triggered, this, [this]() {
        if (portfolio_explorer_sub_window_) {
            ctx_.mdi_area->setActiveSubWindow(portfolio_explorer_sub_window_);
            return;
        }

        auto* window = new PortfolioExplorerMdiWindow(
            ctx_.client_manager,
            bookController_.get(),
            portfolioController_.get(),
            tradeController_.get(),
            nullptr,
            ctx_.username,
            ctx_.main_window);

        connect(window, &PortfolioExplorerMdiWindow::statusChanged,
                this, [this](const QString& msg) { emit status_message(msg); });

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

    auto* actOrgExplorer = menuTrading->addAction(ico(Icon::Organization), tr("&Org Explorer"));
    connect(actOrgExplorer, &QAction::triggered, this, [this]() {
        if (org_explorer_sub_window_) {
            ctx_.mdi_area->setActiveSubWindow(org_explorer_sub_window_);
            return;
        }

        auto* window = new OrgExplorerMdiWindow(
            ctx_.client_manager,
            businessUnitController_.get(),
            bookController_.get(),
            tradeController_.get(),
            ctx_.username,
            ctx_.main_window);

        connect(window, &OrgExplorerMdiWindow::statusChanged,
                this, [this](const QString& msg) { emit status_message(msg); });

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
    auto* actTrades = menuTrading->addAction(ico(Icon::ArrowTrending), tr("&Trades"));
    connect(actTrades, &QAction::triggered, this, [this]() {
        if (tradeController_) tradeController_->showListWindow();
    });
    // ---- Analytics --------------------------------------------------
    auto* menuAnalytics = new QMenu(tr("&Analytics"));

    auto* actPricingEngineTypes = menuAnalytics->addAction(
        ico(Icon::Tag), tr("&Pricing Engine Types"));
    connect(actPricingEngineTypes, &QAction::triggered, this, [this]() {
        if (pricingEngineTypeController_) pricingEngineTypeController_->showListWindow();
    });
    menuAnalytics->addSeparator();
    auto* actPricingModelConfigs = menuAnalytics->addAction(
        ico(Icon::Chart), tr("Pricing &Model Configs"));
    connect(actPricingModelConfigs, &QAction::triggered, this, [this]() {
        if (pricingModelConfigController_) pricingModelConfigController_->showListWindow();
    });
    auto* actPricingModelProducts = menuAnalytics->addAction(
        ico(Icon::Table), tr("Pricing Model &Products"));
    connect(actPricingModelProducts, &QAction::triggered, this, [this]() {
        if (pricingModelProductController_) pricingModelProductController_->showListWindow();
    });
    auto* actPricingModelProductParameters = menuAnalytics->addAction(
        ico(Icon::Settings), tr("Pricing Model Product &Parameters"));
    connect(actPricingModelProductParameters, &QAction::triggered, this, [this]() {
        if (pricingModelProductParameterController_)
            pricingModelProductParameterController_->showListWindow();
    });

    // ---- Market Data ------------------------------------------------
    auto* menuMarketData = new QMenu(tr("&Market Data"));

    auto* actMarketSeries = menuMarketData->addAction(ico(Icon::ChartMultiple), tr("Market &Series"));
    connect(actMarketSeries, &QAction::triggered, this, [this]() {
        if (marketDataController_) marketDataController_->showListWindow();
    });
    auto* actMarketFixings = menuMarketData->addAction(ico(Icon::Chart), tr("Market &Fixings"));
    connect(actMarketFixings, &QAction::triggered, this, [this]() {
        if (marketDataController_) marketDataController_->showFixingsWindow();
    });

    return {menuData, menuTrading, menuAnalytics, menuMarketData};
}

// ---------------------------------------------------------------------------
// IPlugin::on_logout — close open singleton windows, destroy all controllers
// ---------------------------------------------------------------------------
void LegacyPlugin::on_logout() {
    if (portfolio_explorer_sub_window_) {
        portfolio_explorer_sub_window_->close();
        portfolio_explorer_sub_window_ = nullptr;
    }
    if (org_explorer_sub_window_) {
        org_explorer_sub_window_->close();
        org_explorer_sub_window_ = nullptr;
    }
    if (data_librarian_window_) {
        data_librarian_window_->close();
        data_librarian_window_ = nullptr;
    }

    marketDataController_.reset();
    pricingModelProductParameterController_.reset();
    pricingModelProductController_.reset();
    pricingModelConfigController_.reset();
    pricingEngineTypeController_.reset();
    tradeController_.reset();
    currencyMarketTierController_.reset();
    bookStatusController_.reset();
    bookController_.reset();
    portfolioController_.reset();
    businessUnitTypeController_.reset();
    businessUnitController_.reset();
    businessCentreController_.reset();
    counterpartyController_.reset();
    partyController_.reset();
    contactTypeController_.reset();
    partyIdSchemeController_.reset();
    partyStatusController_.reset();
    partyTypeController_.reset();

    ctx_ = {};
}

} // namespace ores::qt
