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
#include "ores.qt/RefdataPlugin.hpp"

#include <QMenu>
#include <QAction>
#include <QMdiArea>
#include <QMdiSubWindow>
#include <QMainWindow>
#include "ores.logging/make_logger.hpp"

#include "ores.qt/IconUtils.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/DataLibrarianWindow.hpp"
#include "ores.qt/CurrencyController.hpp"
#include "ores.qt/CountryController.hpp"
#include "ores.qt/ChangeReasonCategoryController.hpp"
#include "ores.qt/ChangeReasonController.hpp"
#include "ores.qt/CodingSchemeAuthorityTypeController.hpp"
#include "ores.qt/CodeDomainController.hpp"
#include "ores.qt/CodingSchemeController.hpp"
#include "ores.qt/DatasetController.hpp"
#include "ores.qt/DayCountFractionTypeController.hpp"
#include "ores.qt/BusinessDayConventionTypeController.hpp"
#include "ores.qt/FloatingIndexTypeController.hpp"
#include "ores.qt/PaymentFrequencyTypeController.hpp"
#include "ores.qt/LegTypeController.hpp"
#include "ores.qt/MonetaryNatureController.hpp"
#include "ores.qt/RoundingTypeController.hpp"
#include "ores.qt/PurposeTypeController.hpp"
#include "ores.qt/ZeroConventionController.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
auto& lg() {
    static auto instance = make_logger("ores.qt.refdata_plugin");
    return instance;
}
}

RefdataPlugin::RefdataPlugin(QObject* parent) : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

RefdataPlugin::~RefdataPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

// ---------------------------------------------------------------------------
// IPlugin::on_login — create all controllers and wire cross-controller relays
// ---------------------------------------------------------------------------
void RefdataPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    currencyController_ = std::make_unique<CurrencyController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(currencyController_.get());

    countryController_ = std::make_unique<CountryController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.image_cache,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(countryController_.get());

    changeReasonCategoryController_ = std::make_unique<ChangeReasonCategoryController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(changeReasonCategoryController_.get());

    changeReasonController_ = std::make_unique<ChangeReasonController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username,
        ctx_.change_reason_cache, this);
    connectControllerSignals(changeReasonController_.get());

    codingSchemeAuthorityTypeController_ = std::make_unique<CodingSchemeAuthorityTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(codingSchemeAuthorityTypeController_.get());

    codeDomainController_ = std::make_unique<CodeDomainController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.username, this);
    connectControllerSignals(codeDomainController_.get());

    codingSchemeController_ = std::make_unique<CodingSchemeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(codingSchemeController_.get());

    datasetController_ = std::make_unique<DatasetController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(datasetController_.get());

    dayCountFractionTypeController_ = std::make_unique<DayCountFractionTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(dayCountFractionTypeController_.get());

    businessDayConventionTypeController_ = std::make_unique<BusinessDayConventionTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(businessDayConventionTypeController_.get());

    floatingIndexTypeController_ = std::make_unique<FloatingIndexTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(floatingIndexTypeController_.get());

    paymentFrequencyTypeController_ = std::make_unique<PaymentFrequencyTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(paymentFrequencyTypeController_.get());

    legTypeController_ = std::make_unique<LegTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(legTypeController_.get());

    roundingTypeController_ = std::make_unique<RoundingTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(roundingTypeController_.get());

    // CurrencyController cross-domain relays (within refdata)
    connect(currencyController_.get(), &CurrencyController::showRoundingTypesRequested,
            this, [this]() {
        if (roundingTypeController_) roundingTypeController_->showListWindow();
    });

    monetaryNatureController_ = std::make_unique<MonetaryNatureController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(monetaryNatureController_.get());

    connect(currencyController_.get(), &CurrencyController::showMonetaryNaturesRequested,
            this, [this]() {
        if (monetaryNatureController_) monetaryNatureController_->showListWindow();
    });

    purposeTypeController_ = std::make_unique<PurposeTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(purposeTypeController_.get());

    zeroConventionController_ = std::make_unique<ZeroConventionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.username, this);
    connectControllerSignals(zeroConventionController_.get());
}

// ---------------------------------------------------------------------------
// IPlugin::setup_menus — populate the shared Reference Data menu.
// ---------------------------------------------------------------------------
void RefdataPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Registering entries in shared menus."
        << " reference_data=" << (smc.reference_data_menu ? "ok" : "null")
        << " data_transfer=" << (smc.data_transfer_menu ? "ok" : "null")
        << " trading_codes=" << (smc.trading_codes_menu ? "ok" : "null");
    using IC = IconUtils;
    auto ico = [](Icon i) { return IC::createRecoloredIcon(i, IC::DefaultIconColor); };

    // ---- Reference Data menu --------------------------------------------
    auto* ref = smc.reference_data_menu;
    if (ref) {
        act_currencies_ = ref->addAction(ico(Icon::Currency), tr("&Currencies"));
        connect(act_currencies_, &QAction::triggered, this, [this]() {
            if (currencyController_) currencyController_->showListWindow();
        });
        act_countries_ = ref->addAction(ico(Icon::Globe), tr("C&ountries"));
        connect(act_countries_, &QAction::triggered, this, [this]() {
            if (countryController_) countryController_->showListWindow();
        });

        ref->addSeparator();

        // Trading Conventions submenu (unchanged)
        auto* menuConventions = ref->addMenu(tr("Trading &Conventions"));
        auto* actDayCountFractionTypes = menuConventions->addAction(
            ico(Icon::Tag), tr("&Day Count Fraction Types"));
        connect(actDayCountFractionTypes, &QAction::triggered, this, [this]() {
            if (dayCountFractionTypeController_) dayCountFractionTypeController_->showListWindow();
        });
        auto* actBusinessDayConventionTypes = menuConventions->addAction(
            ico(Icon::Tag), tr("&Business Day Convention Types"));
        connect(actBusinessDayConventionTypes, &QAction::triggered, this, [this]() {
            if (businessDayConventionTypeController_)
                businessDayConventionTypeController_->showListWindow();
        });
        auto* actFloatingIndexTypes = menuConventions->addAction(
            ico(Icon::Tag), tr("&Floating Index Types"));
        connect(actFloatingIndexTypes, &QAction::triggered, this, [this]() {
            if (floatingIndexTypeController_) floatingIndexTypeController_->showListWindow();
        });
        auto* actPaymentFrequencyTypes = menuConventions->addAction(
            ico(Icon::Tag), tr("&Payment Frequency Types"));
        connect(actPaymentFrequencyTypes, &QAction::triggered, this, [this]() {
            if (paymentFrequencyTypeController_) paymentFrequencyTypeController_->showListWindow();
        });
        auto* actLegTypes = menuConventions->addAction(ico(Icon::Tag), tr("&Leg Types"));
        connect(actLegTypes, &QAction::triggered, this, [this]() {
            if (legTypeController_) legTypeController_->showListWindow();
        });

        // Conventions submenu (curve-building conventions from conventions.xml)
        auto* menuOreConventions = ref->addMenu(tr("Con&ventions"));
        auto* actZeroConventions = menuOreConventions->addAction(
            ico(Icon::Tag), tr("&Zero Conventions"));
        connect(actZeroConventions, &QAction::triggered, this, [this]() {
            if (zeroConventionController_) zeroConventionController_->showListWindow();
        });

        ref->addSeparator();

        // Classifications submenu
        auto* menuClassifications = ref->addMenu(tr("C&lassifications"));
        auto* actCodingSchemes = menuClassifications->addAction(
            ico(Icon::Code), tr("Codin&g Schemes"));
        connect(actCodingSchemes, &QAction::triggered, this, [this]() {
            if (codingSchemeController_) codingSchemeController_->showListWindow();
        });
        auto* actCodeDomains = menuClassifications->addAction(
            ico(Icon::Tag), tr("Code &Domains"));
        connect(actCodeDomains, &QAction::triggered, this, [this]() {
            if (codeDomainController_) codeDomainController_->showListWindow();
        });
        auto* actCodingSchemeAuthorityTypes = menuClassifications->addAction(
            ico(Icon::Tag), tr("Coding Scheme &Authority Types"));
        connect(actCodingSchemeAuthorityTypes, &QAction::triggered, this, [this]() {
            if (codingSchemeAuthorityTypeController_)
                codingSchemeAuthorityTypeController_->showListWindow();
        });

        ref->addSeparator();

        // Currency Codes submenu (Monetary Natures + Rounding Types)
        auto* menuCurrencyCodes = ref->addMenu(tr("Currency &Codes"));
        auto* actMonetaryNatures = menuCurrencyCodes->addAction(
            ico(Icon::Classification), tr("&Monetary Natures"));
        connect(actMonetaryNatures, &QAction::triggered, this, [this]() {
            if (monetaryNatureController_) monetaryNatureController_->showListWindow();
        });
        auto* actRoundingTypes = menuCurrencyCodes->addAction(
            ico(Icon::Tag), tr("&Rounding Types"));
        connect(actRoundingTypes, &QAction::triggered, this, [this]() {
            if (roundingTypeController_) roundingTypeController_->showListWindow();
        });

        ref->addSeparator();

        // Audit Trail submenu (Change Reason Categories + Change Reasons)
        auto* menuAuditTrail = ref->addMenu(tr("&Audit Trail"));
        auto* actChangeReasonCategories = menuAuditTrail->addAction(
            ico(Icon::Tag), tr("Change Reason &Categories"));
        connect(actChangeReasonCategories, &QAction::triggered, this, [this]() {
            if (changeReasonCategoryController_)
                changeReasonCategoryController_->showListWindow();
        });
        auto* actChangeReasons = menuAuditTrail->addAction(
            ico(Icon::NoteEdit), tr("Change &Reasons"));
        connect(actChangeReasons, &QAction::triggered, this, [this]() {
            if (changeReasonController_) changeReasonController_->showListWindow();
        });
    }

    // ---- Data Transfer menu — contribute Data Librarian action ----------
    auto* dt = smc.data_transfer_menu;
    if (dt) {
        act_data_librarian_ = dt->addAction(
            IconUtils::createRecoloredIcon(Icon::Library, IconUtils::DefaultIconColor),
            tr("Data &Librarian"));
        connect(act_data_librarian_, &QAction::triggered, this, [this]() {
            if (data_librarian_window_) {
                ctx_.mdi_area->setActiveSubWindow(data_librarian_window_);
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
                    this, [this](const QString& msg) { emit statusMessage(msg); });
            connect(librarianWindow, &DataLibrarianWindow::errorOccurred,
                    this, [this](const QString& msg) { emit statusMessage(msg); });

            data_librarian_window_ = subWindow;
            connect(subWindow, &QObject::destroyed, this, [this]() {
                data_librarian_window_ = nullptr;
            });

            ctx_.mdi_area->addSubWindow(subWindow);
            subWindow->resize(librarianWindow->sizeHint());
            subWindow->show();
        });
    }

    // ---- Trading Codes menu — contribute Purpose Types ------------------
    auto* tc = smc.trading_codes_menu;
    if (tc) {
        auto* actPurposeTypes = tc->addAction(
            IconUtils::createRecoloredIcon(Icon::Flag, IconUtils::DefaultIconColor),
            tr("&Purpose Types"));
        connect(actPurposeTypes, &QAction::triggered, this, [this]() {
            if (purposeTypeController_) purposeTypeController_->showListWindow();
        });
    }
}

// ---------------------------------------------------------------------------
QList<QMenu*> RefdataPlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "No standalone menus — all entries contributed via shared menus.";
    return {};  // all items contributed to the shared Reference Data menu
}

QList<QAction*> RefdataPlugin::toolbar_actions() {
    if (!act_currencies_ || !act_countries_)
        BOOST_LOG_SEV(lg(), warn) << "One or more toolbar actions are uninitialised.";
    return {act_currencies_, act_countries_};
}

// ---------------------------------------------------------------------------
// IPlugin::on_logout — destroy all controllers in reverse dependency order
// ---------------------------------------------------------------------------
void RefdataPlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    if (data_librarian_window_) {
        data_librarian_window_->close();
        data_librarian_window_ = nullptr;
    }

    zeroConventionController_.reset();
    purposeTypeController_.reset();
    monetaryNatureController_.reset();
    roundingTypeController_.reset();
    legTypeController_.reset();
    paymentFrequencyTypeController_.reset();
    floatingIndexTypeController_.reset();
    businessDayConventionTypeController_.reset();
    dayCountFractionTypeController_.reset();
    datasetController_.reset();
    codingSchemeController_.reset();
    codeDomainController_.reset();
    codingSchemeAuthorityTypeController_.reset();
    changeReasonController_.reset();
    changeReasonCategoryController_.reset();
    countryController_.reset();
    currencyController_.reset();

    ctx_ = {};
}

} // namespace ores::qt
