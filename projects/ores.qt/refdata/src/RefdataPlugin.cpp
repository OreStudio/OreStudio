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
#include "ores.logging/make_logger.hpp"
#include "ores.qt/BusinessDayConventionTypeController.hpp"
#include "ores.qt/CatalogController.hpp"
#include "ores.qt/CdsConventionController.hpp"
#include "ores.qt/ChangeReasonCategoryController.hpp"
#include "ores.qt/ChangeReasonController.hpp"
#include "ores.qt/CodeDomainController.hpp"
#include "ores.qt/CodingSchemeAuthorityTypeController.hpp"
#include "ores.qt/CodingSchemeController.hpp"
#include "ores.qt/CountryController.hpp"
#include "ores.qt/CurrencyController.hpp"
#include "ores.qt/CurrencyMarketTierController.hpp"
#include "ores.qt/CurrencyPairController.hpp"
#include "ores.qt/CurrencyPairConventionController.hpp"
#include "ores.qt/DataDomainController.hpp"
#include "ores.qt/DataLibrarianWindow.hpp"
#include "ores.qt/DatasetBundleController.hpp"
#include "ores.qt/DatasetController.hpp"
#include "ores.qt/DayCountFractionTypeController.hpp"
#include "ores.qt/DepositConventionController.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/FloatingIndexTypeController.hpp"
#include "ores.qt/FraConventionController.hpp"
#include "ores.qt/IborIndexConventionController.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LegTypeController.hpp"
#include "ores.qt/MethodologyController.hpp"
#include "ores.qt/MonetaryNatureController.hpp"
#include "ores.qt/NatureDimensionController.hpp"
#include "ores.qt/OisConventionController.hpp"
#include "ores.qt/OriginDimensionController.hpp"
#include "ores.qt/OvernightIndexConventionController.hpp"
#include "ores.qt/PartyTypeController.hpp"
#include "ores.qt/PaymentFrequencyTypeController.hpp"
#include "ores.qt/PurposeTypeController.hpp"
#include "ores.qt/RoundingTypeController.hpp"
#include "ores.qt/SubjectAreaController.hpp"
#include "ores.qt/SwapConventionController.hpp"
#include "ores.qt/TreatmentDimensionController.hpp"
#include "ores.qt/ZeroConventionController.hpp"
#include <QAction>
#include <QMainWindow>
#include <QMdiArea>
#include <QMdiSubWindow>
#include <QMenu>

namespace ores::qt {

using namespace ores::logging;

namespace {
auto& lg() {
    static auto instance = make_logger("ores.qt.refdata_plugin");
    return instance;
}
}

RefdataPlugin::RefdataPlugin(QObject* parent)
    : PluginBase(parent) {
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

    currencyController_ = std::make_unique<CurrencyController>(ctx_.main_window,
                                                               ctx_.mdi_area,
                                                               ctx_.client_manager,
                                                               ctx_.image_cache,
                                                               ctx_.change_reason_cache,
                                                               ctx_.username,
                                                               this);
    connectControllerSignals(currencyController_.get());

    countryController_ = std::make_unique<CountryController>(ctx_.main_window,
                                                             ctx_.mdi_area,
                                                             ctx_.client_manager,
                                                             ctx_.image_cache,
                                                             ctx_.change_reason_cache,
                                                             ctx_.username,
                                                             this);
    connectControllerSignals(countryController_.get());

    changeReasonCategoryController_ =
        std::make_unique<ChangeReasonCategoryController>(ctx_.main_window,
                                                         ctx_.mdi_area,
                                                         ctx_.client_manager,
                                                         ctx_.change_reason_cache,
                                                         ctx_.username,
                                                         this);
    connectControllerSignals(changeReasonCategoryController_.get());

    changeReasonController_ = std::make_unique<ChangeReasonController>(ctx_.main_window,
                                                                       ctx_.mdi_area,
                                                                       ctx_.client_manager,
                                                                       ctx_.username,
                                                                       ctx_.change_reason_cache,
                                                                       this);
    connectControllerSignals(changeReasonController_.get());

    codingSchemeAuthorityTypeController_ =
        std::make_unique<CodingSchemeAuthorityTypeController>(ctx_.main_window,
                                                              ctx_.mdi_area,
                                                              ctx_.client_manager,
                                                              ctx_.change_reason_cache,
                                                              ctx_.username,
                                                              this);
    connectControllerSignals(codingSchemeAuthorityTypeController_.get());

    codeDomainController_ = std::make_unique<CodeDomainController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(codeDomainController_.get());

    codingSchemeController_ = std::make_unique<CodingSchemeController>(ctx_.main_window,
                                                                       ctx_.mdi_area,
                                                                       ctx_.client_manager,
                                                                       ctx_.change_reason_cache,
                                                                       ctx_.username,
                                                                       this);
    connectControllerSignals(codingSchemeController_.get());

    datasetController_ = std::make_unique<DatasetController>(ctx_.main_window,
                                                             ctx_.mdi_area,
                                                             ctx_.client_manager,
                                                             ctx_.change_reason_cache,
                                                             ctx_.username,
                                                             this);
    connectControllerSignals(datasetController_.get());

    dayCountFractionTypeController_ = std::make_unique<DayCountFractionTypeController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(dayCountFractionTypeController_.get());

    businessDayConventionTypeController_ =
        std::make_unique<BusinessDayConventionTypeController>(ctx_.main_window,
                                                              ctx_.mdi_area,
                                                              ctx_.client_manager,
                                                              ctx_.change_reason_cache,
                                                              ctx_.username,
                                                              this);
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

    roundingTypeController_ = std::make_unique<RoundingTypeController>(ctx_.main_window,
                                                                       ctx_.mdi_area,
                                                                       ctx_.client_manager,
                                                                       ctx_.change_reason_cache,
                                                                       ctx_.username,
                                                                       this);
    connectControllerSignals(roundingTypeController_.get());

    // CurrencyController cross-domain relays (within refdata)
    connect(
        currencyController_.get(), &CurrencyController::showRoundingTypesRequested, this, [this]() {
            if (roundingTypeController_)
                roundingTypeController_->showListWindow();
        });

    monetaryNatureController_ = std::make_unique<MonetaryNatureController>(ctx_.main_window,
                                                                           ctx_.mdi_area,
                                                                           ctx_.client_manager,
                                                                           ctx_.change_reason_cache,
                                                                           ctx_.username,
                                                                           this);
    connectControllerSignals(monetaryNatureController_.get());

    connect(currencyController_.get(),
            &CurrencyController::showMonetaryNaturesRequested,
            this,
            [this]() {
                if (monetaryNatureController_)
                    monetaryNatureController_->showListWindow();
            });

    currencyMarketTierController_ =
        std::make_unique<CurrencyMarketTierController>(ctx_.main_window,
                                                       ctx_.mdi_area,
                                                       ctx_.client_manager,
                                                       ctx_.change_reason_cache,
                                                       ctx_.username,
                                                       this);
    connectControllerSignals(currencyMarketTierController_.get());

    connect(
        currencyController_.get(), &CurrencyController::showMarketTiersRequested, this, [this]() {
            if (currencyMarketTierController_)
                currencyMarketTierController_->showListWindow();
        });

    purposeTypeController_ = std::make_unique<PurposeTypeController>(ctx_.main_window,
                                                                     ctx_.mdi_area,
                                                                     ctx_.client_manager,
                                                                     ctx_.change_reason_cache,
                                                                     ctx_.username,
                                                                     this);
    connectControllerSignals(purposeTypeController_.get());

    partyTypeController_ = std::make_unique<PartyTypeController>(ctx_.main_window,
                                                                 ctx_.mdi_area,
                                                                 ctx_.client_manager,
                                                                 ctx_.change_reason_cache,
                                                                 ctx_.username,
                                                                 this);
    connectControllerSignals(partyTypeController_.get());

    zeroConventionController_ = std::make_unique<ZeroConventionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(zeroConventionController_.get());

    depositConventionController_ = std::make_unique<DepositConventionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(depositConventionController_.get());

    swapConventionController_ = std::make_unique<SwapConventionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(swapConventionController_.get());

    oisConventionController_ = std::make_unique<OisConventionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(oisConventionController_.get());

    fraConventionController_ = std::make_unique<FraConventionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(fraConventionController_.get());

    iborIndexConventionController_ = std::make_unique<IborIndexConventionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(iborIndexConventionController_.get());

    overnightIndexConventionController_ = std::make_unique<OvernightIndexConventionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(overnightIndexConventionController_.get());

    currencyPairController_ = std::make_unique<CurrencyPairController>(ctx_.main_window,
                                                                       ctx_.mdi_area,
                                                                       ctx_.client_manager,
                                                                       ctx_.image_cache,
                                                                       ctx_.change_reason_cache,
                                                                       ctx_.username,
                                                                       ctx_.badge_cache,
                                                                       this);
    connectControllerSignals(currencyPairController_.get());
    connect(currencyPairController_.get(),
            &CurrencyPairController::showConventionsRequested,
            this,
            [this]() {
                if (currencyPairConventionController_)
                    currencyPairConventionController_->showListWindow();
            });

    currencyPairConventionController_ =
        std::make_unique<CurrencyPairConventionController>(ctx_.main_window,
                                                           ctx_.mdi_area,
                                                           ctx_.client_manager,
                                                           ctx_.image_cache,
                                                           ctx_.change_reason_cache,
                                                           ctx_.username,
                                                           ctx_.badge_cache,
                                                           this);
    connectControllerSignals(currencyPairConventionController_.get());

    cdsConventionController_ = std::make_unique<CdsConventionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager, ctx_.username, this);
    connectControllerSignals(cdsConventionController_.get());

    // Data Catalogue controllers
    dataDomainController_ = std::make_unique<DataDomainController>(ctx_.main_window,
                                                                   ctx_.mdi_area,
                                                                   ctx_.client_manager,
                                                                   ctx_.change_reason_cache,
                                                                   ctx_.username,
                                                                   this);
    connectControllerSignals(dataDomainController_.get());

    subjectAreaController_ = std::make_unique<SubjectAreaController>(ctx_.main_window,
                                                                     ctx_.mdi_area,
                                                                     ctx_.client_manager,
                                                                     ctx_.change_reason_cache,
                                                                     ctx_.username,
                                                                     this);
    connectControllerSignals(subjectAreaController_.get());

    catalogController_ = std::make_unique<CatalogController>(ctx_.main_window,
                                                             ctx_.mdi_area,
                                                             ctx_.client_manager,
                                                             ctx_.change_reason_cache,
                                                             ctx_.username,
                                                             this);
    connectControllerSignals(catalogController_.get());

    datasetBundleController_ = std::make_unique<DatasetBundleController>(ctx_.main_window,
                                                                         ctx_.mdi_area,
                                                                         ctx_.client_manager,
                                                                         ctx_.change_reason_cache,
                                                                         ctx_.username,
                                                                         this);
    connectControllerSignals(datasetBundleController_.get());

    methodologyController_ = std::make_unique<MethodologyController>(ctx_.main_window,
                                                                     ctx_.mdi_area,
                                                                     ctx_.client_manager,
                                                                     ctx_.change_reason_cache,
                                                                     ctx_.username,
                                                                     this);
    connectControllerSignals(methodologyController_.get());

    originDimensionController_ =
        std::make_unique<OriginDimensionController>(ctx_.main_window,
                                                    ctx_.mdi_area,
                                                    ctx_.client_manager,
                                                    ctx_.change_reason_cache,
                                                    ctx_.username,
                                                    this);
    connectControllerSignals(originDimensionController_.get());

    natureDimensionController_ =
        std::make_unique<NatureDimensionController>(ctx_.main_window,
                                                    ctx_.mdi_area,
                                                    ctx_.client_manager,
                                                    ctx_.change_reason_cache,
                                                    ctx_.username,
                                                    this);
    connectControllerSignals(natureDimensionController_.get());

    treatmentDimensionController_ =
        std::make_unique<TreatmentDimensionController>(ctx_.main_window,
                                                       ctx_.mdi_area,
                                                       ctx_.client_manager,
                                                       ctx_.change_reason_cache,
                                                       ctx_.username,
                                                       this);
    connectControllerSignals(treatmentDimensionController_.get());
}

void RefdataPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Registering entries in shared menus."
                               << " reference_data=" << (smc.reference_data_menu ? "ok" : "null")
                               << " data_management=" << (smc.data_management_menu ? "ok" : "null")
                               << " trading_codes=" << (smc.trading_codes_menu ? "ok" : "null");
    using IC = IconUtils;
    auto ico = [](Icon i) {
        return IC::createRecoloredIcon(i, IC::DefaultIconColor);
    };

    // ---- Reference Data menu --------------------------------------------
    reference_data_menu_ = smc.reference_data_menu;
    auto* ref = reference_data_menu_;
    if (ref) {
        act_currencies_ = ref->addAction(ico(Icon::Currency), tr("&Currencies"));
        connect(act_currencies_, &QAction::triggered, this, [this]() {
            if (currencyController_)
                currencyController_->showListWindow();
        });
        act_countries_ = ref->addAction(ico(Icon::Globe), tr("C&ountries"));
        connect(act_countries_, &QAction::triggered, this, [this]() {
            if (countryController_)
                countryController_->showListWindow();
        });
        act_currency_pairs_ = ref->addAction(ico(Icon::Currency), tr("Currency &Pairs"));
        connect(act_currency_pairs_, &QAction::triggered, this, [this]() {
            if (currencyPairController_)
                currencyPairController_->showListWindow();
        });

        ref->addSeparator();

        // Trading Conventions submenu (unchanged)
        auto* menuConventions = ref->addMenu(tr("Trading &Conventions"));
        auto* actDayCountFractionTypes =
            menuConventions->addAction(ico(Icon::Tag), tr("&Day Count Fraction Types"));
        connect(actDayCountFractionTypes, &QAction::triggered, this, [this]() {
            if (dayCountFractionTypeController_)
                dayCountFractionTypeController_->showListWindow();
        });
        auto* actBusinessDayConventionTypes =
            menuConventions->addAction(ico(Icon::Tag), tr("&Business Day Convention Types"));
        connect(actBusinessDayConventionTypes, &QAction::triggered, this, [this]() {
            if (businessDayConventionTypeController_)
                businessDayConventionTypeController_->showListWindow();
        });
        auto* actFloatingIndexTypes =
            menuConventions->addAction(ico(Icon::Tag), tr("&Floating Index Types"));
        connect(actFloatingIndexTypes, &QAction::triggered, this, [this]() {
            if (floatingIndexTypeController_)
                floatingIndexTypeController_->showListWindow();
        });
        auto* actPaymentFrequencyTypes =
            menuConventions->addAction(ico(Icon::Tag), tr("&Payment Frequency Types"));
        connect(actPaymentFrequencyTypes, &QAction::triggered, this, [this]() {
            if (paymentFrequencyTypeController_)
                paymentFrequencyTypeController_->showListWindow();
        });
        auto* actLegTypes = menuConventions->addAction(ico(Icon::Tag), tr("&Leg Types"));
        connect(actLegTypes, &QAction::triggered, this, [this]() {
            if (legTypeController_)
                legTypeController_->showListWindow();
        });

        // Conventions submenu (curve-building conventions from conventions.xml)
        auto* menuOreConventions = ref->addMenu(tr("Con&ventions"));
        auto* actZeroConventions =
            menuOreConventions->addAction(ico(Icon::Tag), tr("&Zero Conventions"));
        connect(actZeroConventions, &QAction::triggered, this, [this]() {
            if (zeroConventionController_)
                zeroConventionController_->showListWindow();
        });
        auto* actDepositConventions =
            menuOreConventions->addAction(ico(Icon::Tag), tr("&Deposit Conventions"));
        connect(actDepositConventions, &QAction::triggered, this, [this]() {
            if (depositConventionController_)
                depositConventionController_->showListWindow();
        });
        auto* actSwapConventions =
            menuOreConventions->addAction(ico(Icon::Tag), tr("&Swap Conventions"));
        connect(actSwapConventions, &QAction::triggered, this, [this]() {
            if (swapConventionController_)
                swapConventionController_->showListWindow();
        });
        auto* actOisConventions =
            menuOreConventions->addAction(ico(Icon::Tag), tr("&OIS Conventions"));
        connect(actOisConventions, &QAction::triggered, this, [this]() {
            if (oisConventionController_)
                oisConventionController_->showListWindow();
        });
        auto* actFraConventions =
            menuOreConventions->addAction(ico(Icon::Tag), tr("&FRA Conventions"));
        connect(actFraConventions, &QAction::triggered, this, [this]() {
            if (fraConventionController_)
                fraConventionController_->showListWindow();
        });
        auto* actIborIndexConventions =
            menuOreConventions->addAction(ico(Icon::Tag), tr("&IBOR Index Conventions"));
        connect(actIborIndexConventions, &QAction::triggered, this, [this]() {
            if (iborIndexConventionController_)
                iborIndexConventionController_->showListWindow();
        });
        auto* actOvernightIndexConventions =
            menuOreConventions->addAction(ico(Icon::Tag), tr("O&vernight Index Conventions"));
        connect(actOvernightIndexConventions, &QAction::triggered, this, [this]() {
            if (overnightIndexConventionController_)
                overnightIndexConventionController_->showListWindow();
        });
        auto* actCurrencyPairConventions =
            menuOreConventions->addAction(ico(Icon::Tag), tr("Currency Pair Conve&ntions"));
        connect(actCurrencyPairConventions, &QAction::triggered, this, [this]() {
            if (currencyPairConventionController_)
                currencyPairConventionController_->showListWindow();
        });
        auto* actCdsConventions =
            menuOreConventions->addAction(ico(Icon::Tag), tr("&CDS Conventions"));
        connect(actCdsConventions, &QAction::triggered, this, [this]() {
            if (cdsConventionController_)
                cdsConventionController_->showListWindow();
        });

        ref->addSeparator();

        // Classifications submenu
        auto* menuClassifications = ref->addMenu(tr("C&lassifications"));
        auto* actCodingSchemes =
            menuClassifications->addAction(ico(Icon::Code), tr("Codin&g Schemes"));
        connect(actCodingSchemes, &QAction::triggered, this, [this]() {
            if (codingSchemeController_)
                codingSchemeController_->showListWindow();
        });
        auto* actCodeDomains = menuClassifications->addAction(ico(Icon::Tag), tr("Code &Domains"));
        connect(actCodeDomains, &QAction::triggered, this, [this]() {
            if (codeDomainController_)
                codeDomainController_->showListWindow();
        });
        auto* actCodingSchemeAuthorityTypes =
            menuClassifications->addAction(ico(Icon::Tag), tr("Coding Scheme &Authority Types"));
        connect(actCodingSchemeAuthorityTypes, &QAction::triggered, this, [this]() {
            if (codingSchemeAuthorityTypeController_)
                codingSchemeAuthorityTypeController_->showListWindow();
        });

        ref->addSeparator();

        // Currency Codes submenu (Monetary Natures + Rounding Types)
        auto* menuCurrencyCodes = ref->addMenu(tr("Currency &Codes"));
        auto* actMonetaryNatures =
            menuCurrencyCodes->addAction(ico(Icon::Classification), tr("&Monetary Natures"));
        connect(actMonetaryNatures, &QAction::triggered, this, [this]() {
            if (monetaryNatureController_)
                monetaryNatureController_->showListWindow();
        });
        auto* actRoundingTypes =
            menuCurrencyCodes->addAction(ico(Icon::Tag), tr("&Rounding Types"));
        connect(actRoundingTypes, &QAction::triggered, this, [this]() {
            if (roundingTypeController_)
                roundingTypeController_->showListWindow();
        });
        auto* actCurrencyMarketTiers =
            menuCurrencyCodes->addAction(ico(Icon::Chart), tr("Currency Market &Tiers"));
        connect(actCurrencyMarketTiers, &QAction::triggered, this, [this]() {
            if (currencyMarketTierController_)
                currencyMarketTierController_->showListWindow();
        });

        // Organisation Codes submenu: shared with ores.qt.party (host-owned,
        // see shared_menus_context::organisation_codes_menu), since
        // party-domain aux types migrate from ores.qt.party to
        // ores.qt.refdata entity-by-entity as each is (re-)commissioned;
        // see Commission: party_type story.
        if (smc.organisation_codes_menu) {
            ref->addMenu(smc.organisation_codes_menu);
            auto* actPartyTypes =
                smc.organisation_codes_menu->addAction(ico(Icon::Tag), tr("Party &Types"));
            connect(actPartyTypes, &QAction::triggered, this, [this]() {
                if (partyTypeController_)
                    partyTypeController_->showListWindow();
            });
        }

        ref->addSeparator();

        // Audit Trail submenu (Change Reason Categories + Change Reasons)
        auto* menuAuditTrail = ref->addMenu(tr("&Audit Trail"));
        auto* actChangeReasonCategories =
            menuAuditTrail->addAction(ico(Icon::Tag), tr("Change Reason &Categories"));
        connect(actChangeReasonCategories, &QAction::triggered, this, [this]() {
            if (changeReasonCategoryController_)
                changeReasonCategoryController_->showListWindow();
        });
        auto* actChangeReasons =
            menuAuditTrail->addAction(ico(Icon::NoteEdit), tr("Change &Reasons"));
        connect(actChangeReasons, &QAction::triggered, this, [this]() {
            if (changeReasonController_)
                changeReasonController_->showListWindow();
        });
    }

    // ---- Data Management menu — contribute Data Catalogue submenu + Data Librarian
    auto* dt = smc.data_management_menu;
    if (dt) {
        auto* menuCatalogue = dt->addMenu(tr("Data Ca&talogue"));

        auto* actDataDomains = menuCatalogue->addAction(ico(Icon::Folder), tr("&Data Domains"));
        connect(actDataDomains, &QAction::triggered, this, [this]() {
            if (dataDomainController_)
                dataDomainController_->showListWindow();
        });

        auto* actSubjectAreas = menuCatalogue->addAction(ico(Icon::Table), tr("&Subject Areas"));
        connect(actSubjectAreas, &QAction::triggered, this, [this]() {
            if (subjectAreaController_)
                subjectAreaController_->showListWindow();
        });

        auto* actCatalogs = menuCatalogue->addAction(ico(Icon::Library), tr("&Catalogues"));
        connect(actCatalogs, &QAction::triggered, this, [this]() {
            if (catalogController_)
                catalogController_->showListWindow();
        });

        auto* actDatasetBundles =
            menuCatalogue->addAction(ico(Icon::Folder), tr("Dataset &Bundles"));
        connect(actDatasetBundles, &QAction::triggered, this, [this]() {
            if (datasetBundleController_)
                datasetBundleController_->showListWindow();
        });

        auto* actMethodologies = menuCatalogue->addAction(ico(Icon::Book), tr("&Methodologies"));
        connect(actMethodologies, &QAction::triggered, this, [this]() {
            if (methodologyController_)
                methodologyController_->showListWindow();
        });

        menuCatalogue->addSeparator();

        auto* actOriginDimensions =
            menuCatalogue->addAction(ico(Icon::Database), tr("&Origin Dimensions"));
        connect(actOriginDimensions, &QAction::triggered, this, [this]() {
            if (originDimensionController_)
                originDimensionController_->showListWindow();
        });

        auto* actNatureDimensions =
            menuCatalogue->addAction(ico(Icon::Database), tr("&Nature Dimensions"));
        connect(actNatureDimensions, &QAction::triggered, this, [this]() {
            if (natureDimensionController_)
                natureDimensionController_->showListWindow();
        });

        auto* actTreatmentDimensions =
            menuCatalogue->addAction(ico(Icon::Database), tr("&Treatment Dimensions"));
        connect(actTreatmentDimensions, &QAction::triggered, this, [this]() {
            if (treatmentDimensionController_)
                treatmentDimensionController_->showListWindow();
        });

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
            subWindow->setWindowIcon(
                IconUtils::createRecoloredIcon(Icon::Library, IconUtils::DefaultIconColor));
            subWindow->setAttribute(Qt::WA_DeleteOnClose);

            connect(librarianWindow,
                    &DataLibrarianWindow::statusChanged,
                    this,
                    [this](const QString& msg) { emit statusMessage(msg); });
            connect(librarianWindow,
                    &DataLibrarianWindow::errorOccurred,
                    this,
                    [this](const QString& msg) { emit statusMessage(msg); });

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
        auto* actPurposeTypes =
            tc->addAction(IconUtils::createRecoloredIcon(Icon::Flag, IconUtils::DefaultIconColor),
                          tr("&Purpose Types"));
        connect(actPurposeTypes, &QAction::triggered, this, [this]() {
            if (purposeTypeController_)
                purposeTypeController_->showListWindow();
        });
    }
}

// ---------------------------------------------------------------------------
QList<QMenu*> RefdataPlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "Returning pre-created Reference Data menu.";
    if (!reference_data_menu_)
        BOOST_LOG_SEV(lg(), warn) << "Reference Data menu not initialised via setup_menus.";
    return reference_data_menu_ ? QList<QMenu*>{reference_data_menu_} : QList<QMenu*>{};
}

QList<QAction*> RefdataPlugin::toolbar_actions() {
    if (!act_currencies_ || !act_countries_ || !act_currency_pairs_)
        BOOST_LOG_SEV(lg(), warn) << "One or more toolbar actions are uninitialised.";
    return {act_currencies_, act_countries_, act_currency_pairs_};
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

    treatmentDimensionController_.reset();
    natureDimensionController_.reset();
    originDimensionController_.reset();
    methodologyController_.reset();
    datasetBundleController_.reset();
    catalogController_.reset();
    subjectAreaController_.reset();
    dataDomainController_.reset();

    zeroConventionController_.reset();
    partyTypeController_.reset();
    purposeTypeController_.reset();
    currencyMarketTierController_.reset();
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
