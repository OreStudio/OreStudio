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
#ifndef ORES_QT_LEGACY_PLUGIN_HPP
#define ORES_QT_LEGACY_PLUGIN_HPP

#include <memory>
#include <QObject>
#include <QList>
#include <QPointer>
#include "ores.qt/IPlugin.hpp"

namespace ores::qt {

class DetachableMdiSubWindow;
class CurrencyController;
class CountryController;
class AccountController;
class RoleController;
class TenantController;
class SystemSettingController;
class ChangeReasonCategoryController;
class ChangeReasonController;
class OriginDimensionController;
class NatureDimensionController;
class TreatmentDimensionController;
class CodingSchemeAuthorityTypeController;
class DataDomainController;
class SubjectAreaController;
class CatalogController;
class CodingSchemeController;
class MethodologyController;
class DatasetController;
class DatasetBundleController;
class PartyTypeController;
class PartyStatusController;
class PartyIdSchemeController;
class ContactTypeController;
class PartyController;
class CounterpartyController;
class BusinessCentreController;
class BusinessUnitController;
class BusinessUnitTypeController;
class JobDefinitionController;
class AppController;
class AppVersionController;
class ComputeDashboardController;
class ComputeConsoleController;
class ServiceDashboardController;
class ReportTypeController;
class ConcurrencyPolicyController;
class ReportDefinitionController;
class ReportInstanceController;
class PortfolioController;
class BookController;
class BookStatusController;
class QueueMonitorController;
class PurposeTypeController;
class RoundingTypeController;
class MonetaryNatureController;
class CurrencyMarketTierController;
class TradeController;
class DayCountFractionTypeController;
class BusinessDayConventionTypeController;
class FloatingIndexTypeController;
class PaymentFrequencyTypeController;
class LegTypeController;
class OreImportController;
class PricingEngineTypeController;
class PricingModelConfigController;
class PricingModelProductController;
class PricingModelProductParameterController;
class MarketDataController;
class PortfolioExplorerMdiWindow;
class OrgExplorerMdiWindow;
class DataLibrarianWindow;

/**
 * @brief Transitional plugin that wraps the entire legacy ores.qt monolith.
 *
 * Holds all domain entity controllers that used to live directly in MainWindow.
 * On login the controllers are created; on logout they are destroyed.  Domain
 * menus are built in code by create_menus() and inserted into the host menu bar.
 *
 * This class will be split into domain-specific plugins in Steps 3–8.
 */
class LegacyPlugin : public QObject, public IPlugin {
    Q_OBJECT

public:
    explicit LegacyPlugin(QObject* parent = nullptr);
    ~LegacyPlugin() override;

    QString name() const override { return QStringLiteral("ores.qt.legacy"); }
    int load_order() const override { return 999; }

    void on_login(const plugin_context& ctx) override;
    QList<QMenu*> create_menus() override;
    void on_logout() override;

    /**
     * @brief Show methods for System menu items owned by the host menu bar.
     *
     * The host wires ui_->ActionXxx to these after on_login().
     */
    void show_accounts();
    void show_roles();
    void show_tenants();
    void show_feature_flags();
    void show_queue_monitor();
    void show_service_dashboard();

signals:
    /** @brief Forwarded status/error messages from entity controllers. */
    void status_message(const QString& msg);

    /** @brief Forwarded window lifecycle signals from entity controllers. */
    void window_created(DetachableMdiSubWindow* window);
    void window_destroyed(DetachableMdiSubWindow* window);

    /** @brief Emitted when TenantController requests the onboarding wizard. */
    void onboard_requested();

private slots:
    void on_status_message(const QString& msg) { emit status_message(msg); }
    void on_window_created(DetachableMdiSubWindow* w) { emit window_created(w); }
    void on_window_destroyed(DetachableMdiSubWindow* w) { emit window_destroyed(w); }

private:
    void connect_controller_signals(QObject* ctrl);

    plugin_context ctx_;

    // Singleton MDI sub-windows (nullptr when not open)
    DetachableMdiSubWindow* portfolio_explorer_sub_window_{nullptr};
    DetachableMdiSubWindow* org_explorer_sub_window_{nullptr};
    DetachableMdiSubWindow* data_librarian_window_{nullptr};

    // Entity controllers
    std::unique_ptr<CurrencyController>                    currencyController_;
    std::unique_ptr<CountryController>                     countryController_;
    std::unique_ptr<AccountController>                     accountController_;
    std::unique_ptr<RoleController>                        roleController_;
    std::unique_ptr<TenantController>                      tenantController_;
    std::unique_ptr<SystemSettingController>               systemSettingController_;
    std::unique_ptr<ChangeReasonCategoryController>        changeReasonCategoryController_;
    std::unique_ptr<ChangeReasonController>                changeReasonController_;
    std::unique_ptr<OriginDimensionController>             originDimensionController_;
    std::unique_ptr<NatureDimensionController>             natureDimensionController_;
    std::unique_ptr<TreatmentDimensionController>          treatmentDimensionController_;
    std::unique_ptr<CodingSchemeAuthorityTypeController>   codingSchemeAuthorityTypeController_;
    std::unique_ptr<DataDomainController>                  dataDomainController_;
    std::unique_ptr<SubjectAreaController>                 subjectAreaController_;
    std::unique_ptr<CatalogController>                     catalogController_;
    std::unique_ptr<CodingSchemeController>                codingSchemeController_;
    std::unique_ptr<MethodologyController>                 methodologyController_;
    std::unique_ptr<DatasetController>                     datasetController_;
    std::unique_ptr<DatasetBundleController>               datasetBundleController_;
    std::unique_ptr<PartyTypeController>                   partyTypeController_;
    std::unique_ptr<PartyStatusController>                 partyStatusController_;
    std::unique_ptr<PartyIdSchemeController>               partyIdSchemeController_;
    std::unique_ptr<ContactTypeController>                 contactTypeController_;
    std::unique_ptr<PartyController>                       partyController_;
    std::unique_ptr<CounterpartyController>                counterpartyController_;
    std::unique_ptr<BusinessCentreController>              businessCentreController_;
    std::unique_ptr<BusinessUnitController>                businessUnitController_;
    std::unique_ptr<BusinessUnitTypeController>            businessUnitTypeController_;
    std::unique_ptr<PortfolioController>                   portfolioController_;
    std::unique_ptr<BookController>                        bookController_;
    std::unique_ptr<BookStatusController>                  bookStatusController_;
    std::unique_ptr<QueueMonitorController>                queueMonitorController_;
    std::unique_ptr<PurposeTypeController>                 purposeTypeController_;
    std::unique_ptr<RoundingTypeController>                roundingTypeController_;
    std::unique_ptr<MonetaryNatureController>              monetaryNatureController_;
    std::unique_ptr<CurrencyMarketTierController>          currencyMarketTierController_;
    std::unique_ptr<TradeController>                       tradeController_;
    std::unique_ptr<DayCountFractionTypeController>        dayCountFractionTypeController_;
    std::unique_ptr<BusinessDayConventionTypeController>   businessDayConventionTypeController_;
    std::unique_ptr<FloatingIndexTypeController>           floatingIndexTypeController_;
    std::unique_ptr<PaymentFrequencyTypeController>        paymentFrequencyTypeController_;
    std::unique_ptr<LegTypeController>                     legTypeController_;
    std::unique_ptr<PricingEngineTypeController>           pricingEngineTypeController_;
    std::unique_ptr<PricingModelConfigController>          pricingModelConfigController_;
    std::unique_ptr<PricingModelProductController>         pricingModelProductController_;
    std::unique_ptr<PricingModelProductParameterController> pricingModelProductParameterController_;
    std::unique_ptr<JobDefinitionController>               jobDefinitionController_;
    std::unique_ptr<AppController>                         appController_;
    std::unique_ptr<AppVersionController>                  appVersionController_;
    std::unique_ptr<ComputeDashboardController>            computeDashboardController_;
    std::unique_ptr<ComputeConsoleController>              computeConsoleController_;
    std::unique_ptr<ServiceDashboardController>            serviceDashboardController_;
    std::unique_ptr<ReportTypeController>                  reportTypeController_;
    std::unique_ptr<ConcurrencyPolicyController>           concurrencyPolicyController_;
    std::unique_ptr<ReportDefinitionController>            reportDefinitionController_;
    std::unique_ptr<ReportInstanceController>              reportInstanceController_;
    std::unique_ptr<OreImportController>                   oreImportController_;
    std::unique_ptr<MarketDataController>                  marketDataController_;
};

}

#endif
