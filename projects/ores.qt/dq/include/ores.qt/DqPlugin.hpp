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
#ifndef ORES_QT_DQ_PLUGIN_HPP
#define ORES_QT_DQ_PLUGIN_HPP

#include "ores.qt/PluginBase.hpp"
#include <QList>
#include <memory>

class QAction;
class QMenu;

namespace ores::qt {

class ArtefactTypeController;
class BadgeDefinitionController;
class BadgeSeverityController;
class CatalogController;
class ChangeReasonCategoryController;
class ChangeReasonController;
class CodeDomainController;
class CodingSchemeAuthorityTypeController;
class CodingSchemeController;
class DataDomainController;
class DatasetBundleController;
class DatasetController;
class DetachableMdiSubWindow;
class LeiEntityController;
class LeiRelationshipController;
class MethodologyController;
class NatureDimensionController;
class OriginDimensionController;
class SubjectAreaController;
class TreatmentDimensionController;

/**
 * @brief Qt plugin owning every entity modeled in ores.dq: badge
 * governance (badge_definition, badge_severity, code_domain, and via
 * CodeDomainDetailDialog's BadgeMappingsTab, badge_mapping), the
 * artefact-import pipeline config (artefact_type), the LEI registry
 * (lei_entity/lei_relationship, read-only browse windows), coding
 * scheme classification (coding_scheme, coding_scheme_authority_type),
 * audit-trail taxonomy (change_reason, change_reason_category), and
 * the data catalogue (catalog, data_domain, dataset, dataset_bundle,
 * methodology, subject_area, origin/nature/treatment_dimension) plus
 * its Data Librarian window.
 *
 * The catalogue/coding-scheme/audit-trail/dimension entities and Data
 * Librarian moved in from the now-decommissioned ores.qt.data_management
 * plugin (see f9b7c9651's note that this consolidation was deferred,
 * and the "Verify ores.dq commissioned entities" test scenario that
 * found the cost of leaving it split: a duplicate, uncoordinated second
 * commissioning of 5 of these entities briefly existed in this plugin
 * alongside data_management's working originals, with no eventing or
 * history-provider wiring — the move keeps the *working* originals
 * verbatim rather than regenerating them.
 *
 * report_definition/synthetic_fx_spot_config are deliberately NOT here:
 * they are DQ-side staging views whose authoritative home is another
 * component (ores.reporting/ores.synthetic respectively) -- see each
 * model's "* Notes"/"* Physical space" sections, which disable
 * ores.cpp.qt for them entirely.
 *
 * Every entity this plugin owns is modeled in ores.dq -- the plugin
 * boundary lines up with the C++ component boundary, per the same
 * convention RefdataPlugin/IamPlugin follow for their components.
 * Owns the pre-created data_quality_menu handle.
 */
class DqPlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit DqPlugin(QObject* parent = nullptr);
    ~DqPlugin() override;

    QString name() const override {
        return QStringLiteral("ores.qt.dq");
    }
    int load_order() const override {
        return 380;
    }

    void on_login(const plugin_context& ctx) override;
    void setup_menus(const shared_menus_context& ctx) override;
    QList<QMenu*> create_menus() override;
    void on_logout() override;

private:
    plugin_context ctx_;

    // The data_quality_menu is pre-created by MainWindow and passed via
    // setup_menus context. We hold a reference to return it from create_menus.
    QMenu* data_quality_menu_{nullptr};

    QAction* act_data_librarian_{nullptr};

    // Singleton MDI sub-window for Data Librarian (nullptr when not open)
    DetachableMdiSubWindow* data_librarian_window_{nullptr};

    std::unique_ptr<ArtefactTypeController> artefactTypeController_;
    std::unique_ptr<BadgeDefinitionController> badgeDefinitionController_;
    std::unique_ptr<BadgeSeverityController> badgeSeverityController_;
    std::unique_ptr<CatalogController> catalogController_;
    std::unique_ptr<ChangeReasonCategoryController> changeReasonCategoryController_;
    std::unique_ptr<ChangeReasonController> changeReasonController_;
    std::unique_ptr<CodeDomainController> codeDomainController_;
    std::unique_ptr<CodingSchemeAuthorityTypeController> codingSchemeAuthorityTypeController_;
    std::unique_ptr<CodingSchemeController> codingSchemeController_;
    std::unique_ptr<DataDomainController> dataDomainController_;
    std::unique_ptr<DatasetBundleController> datasetBundleController_;
    std::unique_ptr<DatasetController> datasetController_;
    std::unique_ptr<LeiEntityController> leiEntityController_;
    std::unique_ptr<LeiRelationshipController> leiRelationshipController_;
    std::unique_ptr<MethodologyController> methodologyController_;
    std::unique_ptr<NatureDimensionController> natureDimensionController_;
    std::unique_ptr<OriginDimensionController> originDimensionController_;
    std::unique_ptr<SubjectAreaController> subjectAreaController_;
    std::unique_ptr<TreatmentDimensionController> treatmentDimensionController_;
};

}

#endif
