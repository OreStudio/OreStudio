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

#include <memory>
#include <QList>
#include "ores.qt/PluginBase.hpp"

class QAction;

namespace ores::qt {

class CurrencyController;
class CountryController;
class ChangeReasonCategoryController;
class ChangeReasonController;
class OriginDimensionController;
class NatureDimensionController;
class TreatmentDimensionController;
class CodingSchemeAuthorityTypeController;
class CodeDomainController;
class DataDomainController;
class SubjectAreaController;
class CatalogController;
class CodingSchemeController;
class MethodologyController;
class DatasetController;
class DatasetBundleController;
class DayCountFractionTypeController;
class BusinessDayConventionTypeController;
class FloatingIndexTypeController;
class PaymentFrequencyTypeController;
class LegTypeController;
class MonetaryNatureController;
class RoundingTypeController;
class PurposeTypeController;

/**
 * @brief Reference data plugin: currencies, countries, dimensions, coding
 *        schemes, datasets, trading conventions, and related types.
 *
 * Extracted from LegacyPlugin in Step 5 of the Qt plugin refactor.
 * Owns the Data, Auxiliary Data, Data Governance, and related menus.
 */
/**
 * @brief Reference data plugin: currencies, countries, dimensions, coding
 *        schemes, datasets, trading conventions, and related types.
 *
 * Loaded as a shared library by QPluginLoader at application startup.
 * Owns the Data, Auxiliary Data, Data Governance, and related menus.
 */
class RefdataPlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit RefdataPlugin(QObject* parent = nullptr);
    ~RefdataPlugin() override;

    QString name() const override { return QStringLiteral("ores.qt.refdata"); }
    int load_order() const override { return 100; }  // setup_menus only; no standalone menus

    void on_login(const plugin_context& ctx) override;
    void setup_menus(QMenu* system_menu, QMenu* reference_data_menu) override;
    QList<QMenu*> create_menus() override;
    QList<QAction*> toolbar_actions() override;
    void on_logout() override;

private:

    plugin_context ctx_;

    QAction* act_currencies_{nullptr};
    QAction* act_countries_{nullptr};

    std::unique_ptr<CurrencyController>                    currencyController_;
    std::unique_ptr<CountryController>                     countryController_;
    std::unique_ptr<ChangeReasonCategoryController>        changeReasonCategoryController_;
    std::unique_ptr<ChangeReasonController>                changeReasonController_;
    std::unique_ptr<OriginDimensionController>             originDimensionController_;
    std::unique_ptr<NatureDimensionController>             natureDimensionController_;
    std::unique_ptr<TreatmentDimensionController>          treatmentDimensionController_;
    std::unique_ptr<CodingSchemeAuthorityTypeController>   codingSchemeAuthorityTypeController_;
    std::unique_ptr<CodeDomainController>                  codeDomainController_;
    std::unique_ptr<DataDomainController>                  dataDomainController_;
    std::unique_ptr<SubjectAreaController>                 subjectAreaController_;
    std::unique_ptr<CatalogController>                     catalogController_;
    std::unique_ptr<CodingSchemeController>                codingSchemeController_;
    std::unique_ptr<MethodologyController>                 methodologyController_;
    std::unique_ptr<DatasetController>                     datasetController_;
    std::unique_ptr<DatasetBundleController>               datasetBundleController_;
    std::unique_ptr<DayCountFractionTypeController>        dayCountFractionTypeController_;
    std::unique_ptr<BusinessDayConventionTypeController>   businessDayConventionTypeController_;
    std::unique_ptr<FloatingIndexTypeController>           floatingIndexTypeController_;
    std::unique_ptr<PaymentFrequencyTypeController>        paymentFrequencyTypeController_;
    std::unique_ptr<LegTypeController>                     legTypeController_;
    std::unique_ptr<MonetaryNatureController>              monetaryNatureController_;
    std::unique_ptr<RoundingTypeController>                roundingTypeController_;
    std::unique_ptr<PurposeTypeController>                 purposeTypeController_;
};

}

#endif
