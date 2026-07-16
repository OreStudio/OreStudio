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
#include "ores.qt/AssetClassCodeController.hpp"
#include "ores.qt/BookController.hpp"
#include "ores.qt/BookPurposeTypeController.hpp"
#include "ores.qt/BookStatusController.hpp"
#include "ores.qt/BusinessCentreController.hpp"
#include "ores.qt/BusinessDayConventionTypeController.hpp"
#include "ores.qt/CalendarController.hpp"
#include "ores.qt/CatalogController.hpp"
#include "ores.qt/CdsConventionController.hpp"
#include "ores.qt/ChangeReasonCategoryController.hpp"
#include "ores.qt/ChangeReasonController.hpp"
#include "ores.qt/CodeDomainController.hpp"
#include "ores.qt/CodingSchemeAuthorityTypeController.hpp"
#include "ores.qt/CodingSchemeController.hpp"
#include "ores.qt/ContactTypeController.hpp"
#include "ores.qt/CounterpartyController.hpp"
#include "ores.qt/CountryController.hpp"
#include "ores.qt/CrmDriverPairController.hpp"
#include "ores.qt/CrmEnabledDerivedPairController.hpp"
#include "ores.qt/CrmTopologyConfigController.hpp"
#include "ores.qt/CurrencyController.hpp"
#include "ores.qt/CurrencyGroupController.hpp"
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
#include "ores.qt/InstrumentCodeController.hpp"
#include "ores.qt/LedgerFeedTypeController.hpp"
#include "ores.qt/LegTypeController.hpp"
#include "ores.qt/MethodologyController.hpp"
#include "ores.qt/MonetaryNatureController.hpp"
#include "ores.qt/NatureDimensionController.hpp"
#include "ores.qt/OisConventionController.hpp"
#include "ores.qt/OriginDimensionController.hpp"
#include "ores.qt/OvernightIndexConventionController.hpp"
#include "ores.qt/PartyController.hpp"
#include "ores.qt/PartyTypeController.hpp"
#include "ores.qt/PaymentFrequencyController.hpp"
#include "ores.qt/PurposeTypeController.hpp"
#include "ores.qt/RegulatoryBookTypeController.hpp"
#include "ores.qt/RoundingTypeController.hpp"
#include "ores.qt/SubjectAreaController.hpp"
#include "ores.qt/SwapConventionController.hpp"
#include "ores.qt/TenorAnchorController.hpp"
#include "ores.qt/TenorController.hpp"
#include "ores.qt/TenorConventionController.hpp"
#include "ores.qt/TenorKindController.hpp"
#include "ores.qt/TenorResolutionAlgorithmController.hpp"
#include "ores.qt/TenorUnitController.hpp"
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
                                                               ctx_.badge_cache,
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

    codeDomainController_ = std::make_unique<CodeDomainController>(ctx_.main_window,
                                                                   ctx_.mdi_area,
                                                                   ctx_.client_manager,
                                                                   ctx_.username,
                                                                   ctx_.badge_cache,
                                                                   this);
    connectControllerSignals(codeDomainController_.get());

    codingSchemeController_ = std::make_unique<CodingSchemeController>(ctx_.main_window,
                                                                       ctx_.mdi_area,
                                                                       ctx_.client_manager,
                                                                       ctx_.change_reason_cache,
                                                                       ctx_.username,
                                                                       this);
    connectControllerSignals(codingSchemeController_.get());

    contactTypeController_ = std::make_unique<ContactTypeController>(ctx_.main_window,
                                                                     ctx_.mdi_area,
                                                                     ctx_.client_manager,
                                                                     ctx_.change_reason_cache,
                                                                     ctx_.username,
                                                                     this);
    connectControllerSignals(contactTypeController_.get());

    datasetController_ = std::make_unique<DatasetController>(ctx_.main_window,
                                                             ctx_.mdi_area,
                                                             ctx_.client_manager,
                                                             ctx_.change_reason_cache,
                                                             ctx_.username,
                                                             this);
    connectControllerSignals(datasetController_.get());

    dayCountFractionTypeController_ =
        std::make_unique<DayCountFractionTypeController>(ctx_.main_window,
                                                         ctx_.mdi_area,
                                                         ctx_.client_manager,
                                                         ctx_.change_reason_cache,
                                                         ctx_.username,
                                                         this);
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

    paymentFrequencyController_ =
        std::make_unique<PaymentFrequencyController>(ctx_.main_window,
                                                     ctx_.mdi_area,
                                                     ctx_.client_manager,
                                                     ctx_.change_reason_cache,
                                                     ctx_.username,
                                                     ctx_.badge_cache,
                                                     this);
    connectControllerSignals(paymentFrequencyController_.get());

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

    // Book, BookStatus, RegulatoryBookType: backend already lives in
    // ores.refdata; owned here (not TradingPlugin) for the same reason --
    // no cross-component leakage. TradingPlugin consumes book_controller()
    // via a non-owning pointer for its composite trading-workflow views
    // (Portfolio/Org Explorer) and its "&Books" menu action.
    bookController_ = std::make_unique<BookController>(ctx_.main_window,
                                                       ctx_.mdi_area,
                                                       ctx_.client_manager,
                                                       ctx_.image_cache,
                                                       ctx_.change_reason_cache,
                                                       ctx_.username,
                                                       ctx_.badge_cache,
                                                       this);
    connectControllerSignals(bookController_.get());

    bookStatusController_ = std::make_unique<BookStatusController>(ctx_.main_window,
                                                                   ctx_.mdi_area,
                                                                   ctx_.client_manager,
                                                                   ctx_.change_reason_cache,
                                                                   ctx_.username,
                                                                   this);
    connectControllerSignals(bookStatusController_.get());

    regulatoryBookTypeController_ =
        std::make_unique<RegulatoryBookTypeController>(ctx_.main_window,
                                                       ctx_.mdi_area,
                                                       ctx_.client_manager,
                                                       ctx_.change_reason_cache,
                                                       ctx_.username,
                                                       this);
    connectControllerSignals(regulatoryBookTypeController_.get());

    bookPurposeTypeController_ =
        std::make_unique<BookPurposeTypeController>(ctx_.main_window,
                                                    ctx_.mdi_area,
                                                    ctx_.client_manager,
                                                    ctx_.change_reason_cache,
                                                    ctx_.username,
                                                    this);
    connectControllerSignals(bookPurposeTypeController_.get());

    ledgerFeedTypeController_ = std::make_unique<LedgerFeedTypeController>(ctx_.main_window,
                                                                           ctx_.mdi_area,
                                                                           ctx_.client_manager,
                                                                           ctx_.change_reason_cache,
                                                                           ctx_.username,
                                                                           this);
    connectControllerSignals(ledgerFeedTypeController_.get());

    // BookController cross-domain relays (within refdata): toolbar buttons
    // on BookMdiWindow open its lookup children's list windows, mirroring
    // CurrencyController's RoundingType/MarketTier relay pattern above.
    connect(bookController_.get(), &BookController::showBookStatusesRequested, this, [this]() {
        if (bookStatusController_)
            bookStatusController_->showListWindow();
    });

    connect(
        bookController_.get(), &BookController::showRegulatoryBookTypesRequested, this, [this]() {
            if (regulatoryBookTypeController_)
                regulatoryBookTypeController_->showListWindow();
        });

    connect(bookController_.get(), &BookController::showBookPurposeTypesRequested, this, [this]() {
        if (bookPurposeTypeController_)
            bookPurposeTypeController_->showListWindow();
    });

    connect(bookController_.get(), &BookController::showLedgerFeedTypesRequested, this, [this]() {
        if (ledgerFeedTypeController_)
            ledgerFeedTypeController_->showListWindow();
    });

    crmTopologyConfigController_ =
        std::make_unique<CrmTopologyConfigController>(ctx_.main_window,
                                                      ctx_.mdi_area,
                                                      ctx_.client_manager,
                                                      ctx_.image_cache,
                                                      ctx_.change_reason_cache,
                                                      ctx_.username,
                                                      ctx_.badge_cache,
                                                      this);
    connectControllerSignals(crmTopologyConfigController_.get());

    crmDriverPairController_ = std::make_unique<CrmDriverPairController>(ctx_.main_window,
                                                                         ctx_.mdi_area,
                                                                         ctx_.client_manager,
                                                                         ctx_.image_cache,
                                                                         ctx_.change_reason_cache,
                                                                         ctx_.username,
                                                                         ctx_.badge_cache,
                                                                         this);
    connectControllerSignals(crmDriverPairController_.get());

    crmEnabledDerivedPairController_ =
        std::make_unique<CrmEnabledDerivedPairController>(ctx_.main_window,
                                                          ctx_.mdi_area,
                                                          ctx_.client_manager,
                                                          ctx_.image_cache,
                                                          ctx_.change_reason_cache,
                                                          ctx_.username,
                                                          ctx_.badge_cache,
                                                          this);
    connectControllerSignals(crmEnabledDerivedPairController_.get());

    counterpartyController_ = std::make_unique<CounterpartyController>(ctx_.main_window,
                                                                       ctx_.mdi_area,
                                                                       ctx_.client_manager,
                                                                       ctx_.image_cache,
                                                                       ctx_.change_reason_cache,
                                                                       ctx_.username,
                                                                       ctx_.badge_cache,
                                                                       this);
    connectControllerSignals(counterpartyController_.get());

    partyController_ = std::make_unique<PartyController>(ctx_.main_window,
                                                         ctx_.mdi_area,
                                                         ctx_.client_manager,
                                                         ctx_.image_cache,
                                                         ctx_.change_reason_cache,
                                                         ctx_.username,
                                                         ctx_.badge_cache,
                                                         this);
    connectControllerSignals(partyController_.get());

    partyTypeController_ = std::make_unique<PartyTypeController>(ctx_.main_window,
                                                                 ctx_.mdi_area,
                                                                 ctx_.client_manager,
                                                                 ctx_.change_reason_cache,
                                                                 ctx_.username,
                                                                 this);
    connectControllerSignals(partyTypeController_.get());

    // BusinessCentre: backend already lives in ores.refdata; owned here
    // (not PartyPlugin) for the same reason as Book -- no cross-component
    // leakage, achieved via regeneration from the model's component=refdata.
    businessCentreController_ = std::make_unique<BusinessCentreController>(ctx_.main_window,
                                                                           ctx_.mdi_area,
                                                                           ctx_.client_manager,
                                                                           ctx_.image_cache,
                                                                           ctx_.change_reason_cache,
                                                                           ctx_.username,
                                                                           this);
    connectControllerSignals(businessCentreController_.get());

    calendarController_ = std::make_unique<CalendarController>(ctx_.main_window,
                                                               ctx_.mdi_area,
                                                               ctx_.client_manager,
                                                               ctx_.change_reason_cache,
                                                               ctx_.username,
                                                               ctx_.badge_cache,
                                                               this);
    connectControllerSignals(calendarController_.get());

    zeroConventionController_ = std::make_unique<ZeroConventionController>(ctx_.main_window,
                                                                           ctx_.mdi_area,
                                                                           ctx_.client_manager,
                                                                           ctx_.change_reason_cache,
                                                                           ctx_.username,
                                                                           this);
    connectControllerSignals(zeroConventionController_.get());

    depositConventionController_ =
        std::make_unique<DepositConventionController>(ctx_.main_window,
                                                      ctx_.mdi_area,
                                                      ctx_.client_manager,
                                                      ctx_.change_reason_cache,
                                                      ctx_.username,
                                                      this);
    connectControllerSignals(depositConventionController_.get());

    swapConventionController_ = std::make_unique<SwapConventionController>(ctx_.main_window,
                                                                           ctx_.mdi_area,
                                                                           ctx_.client_manager,
                                                                           ctx_.change_reason_cache,
                                                                           ctx_.username,
                                                                           this);
    connectControllerSignals(swapConventionController_.get());

    oisConventionController_ = std::make_unique<OisConventionController>(ctx_.main_window,
                                                                         ctx_.mdi_area,
                                                                         ctx_.client_manager,
                                                                         ctx_.change_reason_cache,
                                                                         ctx_.username,
                                                                         this);
    connectControllerSignals(oisConventionController_.get());

    fraConventionController_ = std::make_unique<FraConventionController>(ctx_.main_window,
                                                                         ctx_.mdi_area,
                                                                         ctx_.client_manager,
                                                                         ctx_.change_reason_cache,
                                                                         ctx_.username,
                                                                         this);
    connectControllerSignals(fraConventionController_.get());

    iborIndexConventionController_ =
        std::make_unique<IborIndexConventionController>(ctx_.main_window,
                                                        ctx_.mdi_area,
                                                        ctx_.client_manager,
                                                        ctx_.change_reason_cache,
                                                        ctx_.username,
                                                        this);
    connectControllerSignals(iborIndexConventionController_.get());

    overnightIndexConventionController_ =
        std::make_unique<OvernightIndexConventionController>(ctx_.main_window,
                                                             ctx_.mdi_area,
                                                             ctx_.client_manager,
                                                             ctx_.change_reason_cache,
                                                             ctx_.username,
                                                             this);
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

    currencyGroupController_ = std::make_unique<CurrencyGroupController>(ctx_.main_window,
                                                                         ctx_.mdi_area,
                                                                         ctx_.client_manager,
                                                                         ctx_.change_reason_cache,
                                                                         ctx_.username,
                                                                         this);
    connectControllerSignals(currencyGroupController_.get());

    cdsConventionController_ = std::make_unique<CdsConventionController>(ctx_.main_window,
                                                                         ctx_.mdi_area,
                                                                         ctx_.client_manager,
                                                                         ctx_.change_reason_cache,
                                                                         ctx_.username,
                                                                         this);
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

    tenorController_ = std::make_unique<TenorController>(ctx_.main_window,
                                                         ctx_.mdi_area,
                                                         ctx_.client_manager,
                                                         ctx_.change_reason_cache,
                                                         ctx_.username,
                                                         ctx_.badge_cache,
                                                         this);
    connectControllerSignals(tenorController_.get());
    connect(tenorController_.get(), &TenorController::showConventionsRequested, this, [this]() {
        if (tenorConventionController_)
            tenorConventionController_->showListWindow();
    });
    connect(tenorController_.get(), &TenorController::showAnchorsRequested, this, [this]() {
        if (tenorAnchorController_)
            tenorAnchorController_->showListWindow();
    });

    tenorConventionController_ =
        std::make_unique<TenorConventionController>(ctx_.main_window,
                                                    ctx_.mdi_area,
                                                    ctx_.client_manager,
                                                    ctx_.change_reason_cache,
                                                    ctx_.username,
                                                    this);
    connectControllerSignals(tenorConventionController_.get());
    connect(tenorConventionController_.get(),
            &TenorConventionController::showAnchorsRequested,
            this,
            [this]() {
                if (tenorAnchorController_)
                    tenorAnchorController_->showListWindow();
            });
    connect(tenorConventionController_.get(),
            &TenorConventionController::showResolutionAlgorithmsRequested,
            this,
            [this]() {
                if (tenorResolutionAlgorithmController_)
                    tenorResolutionAlgorithmController_->showListWindow();
            });

    tenorAnchorController_ = std::make_unique<TenorAnchorController>(ctx_.main_window,
                                                                     ctx_.mdi_area,
                                                                     ctx_.client_manager,
                                                                     ctx_.change_reason_cache,
                                                                     ctx_.username,
                                                                     this);
    connectControllerSignals(tenorAnchorController_.get());

    tenorKindController_ = std::make_unique<TenorKindController>(ctx_.main_window,
                                                                 ctx_.mdi_area,
                                                                 ctx_.client_manager,
                                                                 ctx_.change_reason_cache,
                                                                 ctx_.username,
                                                                 this);
    connectControllerSignals(tenorKindController_.get());

    tenorUnitController_ = std::make_unique<TenorUnitController>(ctx_.main_window,
                                                                 ctx_.mdi_area,
                                                                 ctx_.client_manager,
                                                                 ctx_.change_reason_cache,
                                                                 ctx_.username,
                                                                 this);
    connectControllerSignals(tenorUnitController_.get());

    tenorResolutionAlgorithmController_ =
        std::make_unique<TenorResolutionAlgorithmController>(ctx_.main_window,
                                                             ctx_.mdi_area,
                                                             ctx_.client_manager,
                                                             ctx_.change_reason_cache,
                                                             ctx_.username,
                                                             this);
    connectControllerSignals(tenorResolutionAlgorithmController_.get());

    assetClassCodeController_ = std::make_unique<AssetClassCodeController>(ctx_.main_window,
                                                                           ctx_.mdi_area,
                                                                           ctx_.client_manager,
                                                                           ctx_.change_reason_cache,
                                                                           ctx_.username,
                                                                           this);
    connectControllerSignals(assetClassCodeController_.get());

    instrumentCodeController_ = std::make_unique<InstrumentCodeController>(ctx_.main_window,
                                                                           ctx_.mdi_area,
                                                                           ctx_.client_manager,
                                                                           ctx_.change_reason_cache,
                                                                           ctx_.username,
                                                                           ctx_.badge_cache,
                                                                           this);
    connectControllerSignals(instrumentCodeController_.get());
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
        act_books_ = ref->addAction(ico(Icon::BookOpen), tr("&Books"));
        connect(act_books_, &QAction::triggered, this, [this]() {
            if (bookController_)
                bookController_->showListWindow();
        });
        act_business_centres_ = ref->addAction(ico(Icon::BuildingBank), tr("&Business Centres"));
        connect(act_business_centres_, &QAction::triggered, this, [this]() {
            if (businessCentreController_)
                businessCentreController_->showListWindow();
        });
        act_calendars_ = ref->addAction(ico(Icon::CalendarClock), tr("Ca&lendars"));
        connect(act_calendars_, &QAction::triggered, this, [this]() {
            if (calendarController_)
                calendarController_->showListWindow();
        });
        act_currency_groups_ = ref->addAction(ico(Icon::Currency), tr("Currency &Groups"));
        connect(act_currency_groups_, &QAction::triggered, this, [this]() {
            if (currencyGroupController_)
                currencyGroupController_->showListWindow();
        });

        act_parties_ = ref->addAction(ico(Icon::Organization), tr("&Parties"));
        connect(act_parties_, &QAction::triggered, this, [this]() {
            if (partyController_)
                partyController_->showListWindow();
        });

        act_counterparties_ = ref->addAction(ico(Icon::Handshake), tr("&Counterparties"));
        connect(act_counterparties_, &QAction::triggered, this, [this]() {
            if (counterpartyController_)
                counterpartyController_->showListWindow();
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
        auto* actPaymentFrequencies =
            menuConventions->addAction(ico(Icon::Clock), tr("Payment Fre&quencies"));
        connect(actPaymentFrequencies, &QAction::triggered, this, [this]() {
            if (paymentFrequencyController_)
                paymentFrequencyController_->showListWindow();
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

        // Tenors submenu: primary entities (Tenors, Tenor Conventions) at the
        // top; auxiliary code lookups (Anchors, Kinds, Units, Resolution
        // Algorithms) directly below a separator, not nested in a submenu.
        auto* menuTenors = ref->addMenu(tr("&Tenors"));
        auto* actTenors = menuTenors->addAction(ico(Icon::Tag), tr("&Tenors"));
        connect(actTenors, &QAction::triggered, this, [this]() {
            if (tenorController_)
                tenorController_->showListWindow();
        });
        auto* actTenorConventions = menuTenors->addAction(ico(Icon::Tag), tr("Tenor &Conventions"));
        connect(actTenorConventions, &QAction::triggered, this, [this]() {
            if (tenorConventionController_)
                tenorConventionController_->showListWindow();
        });

        menuTenors->addSeparator();

        auto* actTenorAnchors = menuTenors->addAction(ico(Icon::Anchor), tr("Tenor &Anchors"));
        connect(actTenorAnchors, &QAction::triggered, this, [this]() {
            if (tenorAnchorController_)
                tenorAnchorController_->showListWindow();
        });
        auto* actTenorKinds = menuTenors->addAction(ico(Icon::Tag), tr("Tenor &Kinds"));
        connect(actTenorKinds, &QAction::triggered, this, [this]() {
            if (tenorKindController_)
                tenorKindController_->showListWindow();
        });
        auto* actTenorUnits = menuTenors->addAction(ico(Icon::Tag), tr("Tenor &Units"));
        connect(actTenorUnits, &QAction::triggered, this, [this]() {
            if (tenorUnitController_)
                tenorUnitController_->showListWindow();
        });
        auto* actTenorResolutionAlgorithms =
            menuTenors->addAction(ico(Icon::Tag), tr("Tenor &Resolution Algorithms"));
        connect(actTenorResolutionAlgorithms, &QAction::triggered, this, [this]() {
            if (tenorResolutionAlgorithmController_)
                tenorResolutionAlgorithmController_->showListWindow();
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
        auto* actAssetClassCodes =
            menuClassifications->addAction(ico(Icon::Tag), tr("Asset &Class Codes"));
        connect(actAssetClassCodes, &QAction::triggered, this, [this]() {
            if (assetClassCodeController_)
                assetClassCodeController_->showListWindow();
        });
        auto* actInstrumentCodes =
            menuClassifications->addAction(ico(Icon::Tag), tr("&Instrument Codes"));
        connect(actInstrumentCodes, &QAction::triggered, this, [this]() {
            if (instrumentCodeController_)
                instrumentCodeController_->showListWindow();
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

        // Book Codes submenu: auxiliary/classification data for books and
        // portfolios (Book Statuses, Regulatory Book Types, Book Purpose
        // Types, Ledger Feed Types today; room for portfolio-side codes as
        // they land). Reference-data lookups belong here, not in the
        // Trading menu's Trading Codes submenu.
        auto* menuBookCodes = ref->addMenu(tr("Book &Codes"));
        auto* actBookStatuses = menuBookCodes->addAction(ico(Icon::Flag), tr("Book &Statuses"));
        connect(actBookStatuses, &QAction::triggered, this, [this]() {
            if (bookStatusController_)
                bookStatusController_->showListWindow();
        });
        auto* actRegulatoryBookTypes =
            menuBookCodes->addAction(ico(Icon::Flag), tr("Regulatory Book &Types"));
        connect(actRegulatoryBookTypes, &QAction::triggered, this, [this]() {
            if (regulatoryBookTypeController_)
                regulatoryBookTypeController_->showListWindow();
        });
        auto* actBookPurposeTypes =
            menuBookCodes->addAction(ico(Icon::Flag), tr("Book &Purpose Types"));
        connect(actBookPurposeTypes, &QAction::triggered, this, [this]() {
            if (bookPurposeTypeController_)
                bookPurposeTypeController_->showListWindow();
        });
        auto* actLedgerFeedTypes =
            menuBookCodes->addAction(ico(Icon::Flag), tr("&Ledger Feed Types"));
        connect(actLedgerFeedTypes, &QAction::triggered, this, [this]() {
            if (ledgerFeedTypeController_)
                ledgerFeedTypeController_->showListWindow();
        });

        // Cross Rates Matrix submenu: configuration data (changes
        // infrequently, curated per party) that drives ores.marketdata's
        // rate_engine but is not itself live market data -- see the
        // reclassification decision on the codegen task doc.
        auto* menuCrossRatesMatrix = ref->addMenu(tr("Cross Rates &Matrix"));
        auto* actCrmTopology = menuCrossRatesMatrix->addAction(ico(Icon::Chart), tr("&Topology"));
        connect(actCrmTopology, &QAction::triggered, this, [this]() {
            if (crmTopologyConfigController_)
                crmTopologyConfigController_->showListWindow();
        });
        auto* actCrmDriverPairs =
            menuCrossRatesMatrix->addAction(ico(Icon::ArrowSync), tr("&Driver Pairs"));
        connect(actCrmDriverPairs, &QAction::triggered, this, [this]() {
            if (crmDriverPairController_)
                crmDriverPairController_->showListWindow();
        });
        auto* actCrmEnabledDerivedPairs =
            menuCrossRatesMatrix->addAction(ico(Icon::ArrowSync), tr("&Enabled Derived Pairs"));
        connect(actCrmEnabledDerivedPairs, &QAction::triggered, this, [this]() {
            if (crmEnabledDerivedPairController_)
                crmEnabledDerivedPairController_->showListWindow();
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
            auto* actContactTypes = smc.organisation_codes_menu->addAction(
                ico(Icon::PersonAccounts), tr("&Contact Types"));
            connect(actContactTypes, &QAction::triggered, this, [this]() {
                if (contactTypeController_)
                    contactTypeController_->showListWindow();
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
    // Book Statuses and Regulatory Book Types are reference-data
    // classification lookups, not trading codes -- they live in the
    // Reference Data menu's "Book Codes" submenu instead (see create_menus()).
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
    if (!act_currencies_ || !act_countries_ || !act_currency_pairs_ || !act_books_ ||
        !act_business_centres_ || !act_parties_ || !act_counterparties_)
        BOOST_LOG_SEV(lg(), warn) << "One or more toolbar actions are uninitialised.";
    return {act_currencies_,
            act_countries_,
            act_currency_pairs_,
            act_books_,
            act_business_centres_,
            act_parties_,
            act_counterparties_};
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

    instrumentCodeController_.reset();
    assetClassCodeController_.reset();
    tenorResolutionAlgorithmController_.reset();
    tenorUnitController_.reset();
    tenorKindController_.reset();
    tenorAnchorController_.reset();
    tenorConventionController_.reset();
    tenorController_.reset();

    treatmentDimensionController_.reset();
    natureDimensionController_.reset();
    originDimensionController_.reset();
    methodologyController_.reset();
    datasetBundleController_.reset();
    catalogController_.reset();
    subjectAreaController_.reset();
    dataDomainController_.reset();

    zeroConventionController_.reset();
    businessCentreController_.reset();
    counterpartyController_.reset();
    partyController_.reset();
    partyTypeController_.reset();
    purposeTypeController_.reset();
    ledgerFeedTypeController_.reset();
    crmTopologyConfigController_.reset();
    crmDriverPairController_.reset();
    crmEnabledDerivedPairController_.reset();
    bookPurposeTypeController_.reset();
    regulatoryBookTypeController_.reset();
    bookStatusController_.reset();
    bookController_.reset();
    currencyMarketTierController_.reset();
    monetaryNatureController_.reset();
    roundingTypeController_.reset();
    legTypeController_.reset();
    paymentFrequencyController_.reset();
    floatingIndexTypeController_.reset();
    businessDayConventionTypeController_.reset();
    dayCountFractionTypeController_.reset();
    datasetController_.reset();
    contactTypeController_.reset();
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
