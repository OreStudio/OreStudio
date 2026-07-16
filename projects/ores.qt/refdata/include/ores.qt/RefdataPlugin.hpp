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
#ifndef ORES_QT_REFDATA_PLUGIN_HPP
#define ORES_QT_REFDATA_PLUGIN_HPP

#include "ores.qt/PluginBase.hpp"
#include "ores.qt/RefdataExport.hpp"
#include <QList>
#include <memory>

class QAction;

namespace ores::qt {

class DataLibrarianWindow;
class DetachableMdiSubWindow;
class DataDomainController;
class SubjectAreaController;
class CatalogController;
class DatasetBundleController;
class MethodologyController;
class OriginDimensionController;
class NatureDimensionController;
class TreatmentDimensionController;
class CurrencyController;
class CountryController;
class ChangeReasonCategoryController;
class ChangeReasonController;
class CodingSchemeAuthorityTypeController;
class CodeDomainController;
class CodingSchemeController;
class ContactTypeController;
class DatasetController;
class DayCountFractionTypeController;
class BusinessDayConventionTypeController;
class FloatingIndexTypeController;
class PaymentFrequencyController;
class LegTypeController;
class CurrencyMarketTierController;
class MonetaryNatureController;
class RoundingTypeController;
class PurposeTypeController;
class ZeroConventionController;
class DepositConventionController;
class SwapConventionController;
class OisConventionController;
class FraConventionController;
class IborIndexConventionController;
class OvernightIndexConventionController;
class CurrencyPairController;
class CurrencyPairConventionController;
class CurrencyGroupController;
class CalendarController;
class CdsConventionController;
class PartyTypeController;
class BusinessCentreController;
class BookController;
class BookStatusController;
class RegulatoryBookTypeController;
class BookPurposeTypeController;
class LedgerFeedTypeController;
class TenorController;
class TenorConventionController;
class TenorAnchorController;
class TenorKindController;
class AssetClassCodeController;
class InstrumentCodeController;
class TenorUnitController;
class TenorResolutionAlgorithmController;
class CrmTopologyConfigController;
class CrmDriverPairController;
class CrmEnabledDerivedPairController;
class CounterpartyController;
class PartyController;

/**
 * @brief Reference data plugin: currencies, countries, dimensions, coding
 *        schemes, datasets, trading conventions, and related types.
 *
 * Loaded as a shared library by QPluginLoader at application startup.
 * Owns the Reference Data, Data Transfer catalogue, and related menus.
 */
class ORES_QT_REFDATA_EXPORT RefdataPlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit RefdataPlugin(QObject* parent = nullptr);
    ~RefdataPlugin() override;

    QString name() const override {
        return QStringLiteral("ores.qt.refdata");
    }
    int load_order() const override {
        return 100;
    } // setup_menus only; no standalone menus

    void on_login(const plugin_context& ctx) override;
    void setup_menus(const shared_menus_context& ctx) override;
    QList<QMenu*> create_menus() override;
    QList<QAction*> toolbar_actions() override;
    void on_logout() override;

    /**
     * @brief Non-owning access to the Book controller for other plugins
     * (e.g. TradingPlugin's Portfolio/Org Explorer views) that consume it
     * without constructing or owning it -- Book's backend and Qt CRUD
     * both live in ores.refdata/ores.qt.refdata; only composite
     * trading-workflow views built on top of it belong to TradingPlugin.
     */
    BookController* book_controller() const noexcept {
        return bookController_.get();
    }

private:
    plugin_context ctx_;

    QMenu* reference_data_menu_{nullptr};

    QAction* act_currencies_{nullptr};
    QAction* act_countries_{nullptr};
    QAction* act_currency_pairs_{nullptr};
    QAction* act_books_{nullptr};
    QAction* act_business_centres_{nullptr};
    QAction* act_calendars_{nullptr};
    QAction* act_currency_groups_{nullptr};
    QAction* act_counterparties_{nullptr};
    QAction* act_parties_{nullptr};
    QAction* act_data_librarian_{nullptr};

    // Singleton MDI sub-window for Data Librarian (nullptr when not open)
    DetachableMdiSubWindow* data_librarian_window_{nullptr};

    std::unique_ptr<CurrencyController> currencyController_;
    std::unique_ptr<CountryController> countryController_;
    std::unique_ptr<ChangeReasonCategoryController> changeReasonCategoryController_;
    std::unique_ptr<ChangeReasonController> changeReasonController_;
    std::unique_ptr<CodingSchemeAuthorityTypeController> codingSchemeAuthorityTypeController_;
    std::unique_ptr<CodeDomainController> codeDomainController_;
    std::unique_ptr<CodingSchemeController> codingSchemeController_;
    std::unique_ptr<ContactTypeController> contactTypeController_;
    std::unique_ptr<DatasetController> datasetController_;
    std::unique_ptr<DayCountFractionTypeController> dayCountFractionTypeController_;
    std::unique_ptr<BusinessDayConventionTypeController> businessDayConventionTypeController_;
    std::unique_ptr<FloatingIndexTypeController> floatingIndexTypeController_;
    std::unique_ptr<PaymentFrequencyController> paymentFrequencyController_;
    std::unique_ptr<LegTypeController> legTypeController_;
    std::unique_ptr<CurrencyMarketTierController> currencyMarketTierController_;
    std::unique_ptr<MonetaryNatureController> monetaryNatureController_;
    std::unique_ptr<RoundingTypeController> roundingTypeController_;
    std::unique_ptr<PurposeTypeController> purposeTypeController_;
    std::unique_ptr<PartyTypeController> partyTypeController_;
    std::unique_ptr<BusinessCentreController> businessCentreController_;
    std::unique_ptr<ZeroConventionController> zeroConventionController_;
    std::unique_ptr<DepositConventionController> depositConventionController_;
    std::unique_ptr<SwapConventionController> swapConventionController_;
    std::unique_ptr<OisConventionController> oisConventionController_;
    std::unique_ptr<FraConventionController> fraConventionController_;
    std::unique_ptr<IborIndexConventionController> iborIndexConventionController_;
    std::unique_ptr<OvernightIndexConventionController> overnightIndexConventionController_;
    std::unique_ptr<CurrencyPairController> currencyPairController_;
    std::unique_ptr<CurrencyGroupController> currencyGroupController_;
    std::unique_ptr<CalendarController> calendarController_;
    std::unique_ptr<CurrencyPairConventionController> currencyPairConventionController_;
    std::unique_ptr<CdsConventionController> cdsConventionController_;
    std::unique_ptr<BookController> bookController_;
    std::unique_ptr<BookStatusController> bookStatusController_;
    std::unique_ptr<RegulatoryBookTypeController> regulatoryBookTypeController_;
    std::unique_ptr<BookPurposeTypeController> bookPurposeTypeController_;
    std::unique_ptr<LedgerFeedTypeController> ledgerFeedTypeController_;
    std::unique_ptr<TenorController> tenorController_;
    std::unique_ptr<TenorConventionController> tenorConventionController_;
    std::unique_ptr<TenorAnchorController> tenorAnchorController_;
    std::unique_ptr<TenorKindController> tenorKindController_;
    std::unique_ptr<AssetClassCodeController> assetClassCodeController_;
    std::unique_ptr<InstrumentCodeController> instrumentCodeController_;
    std::unique_ptr<TenorUnitController> tenorUnitController_;
    std::unique_ptr<TenorResolutionAlgorithmController> tenorResolutionAlgorithmController_;
    std::unique_ptr<CrmTopologyConfigController> crmTopologyConfigController_;
    std::unique_ptr<CrmDriverPairController> crmDriverPairController_;
    std::unique_ptr<CrmEnabledDerivedPairController> crmEnabledDerivedPairController_;
    std::unique_ptr<CounterpartyController> counterpartyController_;
    std::unique_ptr<PartyController> partyController_;

    // Data Catalogue controllers (owned here, contributed to data_management_menu)
    std::unique_ptr<DataDomainController> dataDomainController_;
    std::unique_ptr<SubjectAreaController> subjectAreaController_;
    std::unique_ptr<CatalogController> catalogController_;
    std::unique_ptr<DatasetBundleController> datasetBundleController_;
    std::unique_ptr<MethodologyController> methodologyController_;
    std::unique_ptr<OriginDimensionController> originDimensionController_;
    std::unique_ptr<NatureDimensionController> natureDimensionController_;
    std::unique_ptr<TreatmentDimensionController> treatmentDimensionController_;
};

}

#endif
