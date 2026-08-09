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
#include "ores.qt/DqPlugin.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ArtefactTypeController.hpp"
#include "ores.qt/BadgeDefinitionController.hpp"
#include "ores.qt/BadgeSeverityController.hpp"
#include "ores.qt/CatalogController.hpp"
#include "ores.qt/ChangeReasonCategoryController.hpp"
#include "ores.qt/ChangeReasonController.hpp"
#include "ores.qt/CodeDomainController.hpp"
#include "ores.qt/CodingSchemeAuthorityTypeController.hpp"
#include "ores.qt/CodingSchemeController.hpp"
#include "ores.qt/DataDomainController.hpp"
#include "ores.qt/DataLibrarianWindow.hpp"
#include "ores.qt/DatasetBundleController.hpp"
#include "ores.qt/DatasetController.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MethodologyController.hpp"
#include "ores.qt/NatureDimensionController.hpp"
#include "ores.qt/OriginDimensionController.hpp"
#include "ores.qt/SubjectAreaController.hpp"
#include "ores.qt/TreatmentDimensionController.hpp"
#include <QAction>
#include <QMainWindow>
#include <QMdiArea>
#include <QMenu>

namespace ores::qt {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.qt.dq_plugin");
    return instance;
}

auto ico(Icon icon) {
    return IconUtils::createRecoloredIcon(icon, IconUtils::DefaultIconColor);
}

}

DqPlugin::DqPlugin(QObject* parent)
    : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

DqPlugin::~DqPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

void DqPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    badgeDefinitionController_ =
        std::make_unique<BadgeDefinitionController>(ctx_.main_window,
                                                    ctx_.mdi_area,
                                                    ctx_.client_manager,
                                                    ctx_.change_reason_cache,
                                                    ctx_.username,
                                                    this);
    connectControllerSignals(badgeDefinitionController_.get());

    badgeSeverityController_ = std::make_unique<BadgeSeverityController>(ctx_.main_window,
                                                                         ctx_.mdi_area,
                                                                         ctx_.client_manager,
                                                                         ctx_.change_reason_cache,
                                                                         ctx_.username,
                                                                         this);
    connectControllerSignals(badgeSeverityController_.get());

    codeDomainController_ = std::make_unique<CodeDomainController>(ctx_.main_window,
                                                                   ctx_.mdi_area,
                                                                   ctx_.client_manager,
                                                                   ctx_.change_reason_cache,
                                                                   ctx_.username,
                                                                   this);
    connectControllerSignals(codeDomainController_.get());

    artefactTypeController_ = std::make_unique<ArtefactTypeController>(ctx_.main_window,
                                                                       ctx_.mdi_area,
                                                                       ctx_.client_manager,
                                                                       ctx_.change_reason_cache,
                                                                       ctx_.username,
                                                                       this);
    connectControllerSignals(artefactTypeController_.get());

    // BadgeDefinitionController cross-domain relays: toolbar buttons on
    // BadgeDefinitionMdiWindow open the related badge catalogue windows,
    // mirroring RefdataPlugin's BookController relay pattern. Mappings are
    // browsed via the Code Domains list (see BadgeMappingsTab) since
    // badge_mapping has no standalone list window of its own.
    connect(badgeDefinitionController_.get(),
            &BadgeDefinitionController::showBadgeSeveritiesRequested,
            this,
            [this]() {
                if (badgeSeverityController_)
                    badgeSeverityController_->showListWindow();
            });
    connect(badgeDefinitionController_.get(),
            &BadgeDefinitionController::showBadgeMappingsRequested,
            this,
            [this]() {
                if (codeDomainController_)
                    codeDomainController_->showListWindow();
            });

    // ---- Entities that moved in from ores.qt.data_management ----------

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
                                                                       ctx_.change_reason_cache,
                                                                       ctx_.username,
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

void DqPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Capturing shared Data Quality menu handle."
                               << " data_quality=" << (smc.data_quality_menu ? "ok" : "null")
                               << " classifications=" << (smc.coding_schemes_menu ? "ok" : "null")
                               << " data_transfer=" << (smc.data_transfer_menu ? "ok" : "null");
    data_quality_menu_ = smc.data_quality_menu;
    auto* dq = data_quality_menu_;
    if (!dq)
        return;

    // &Badges submenu: every badge_definition/badge_severity/badge_mapping
    // entry point lives here, not loose at the Data Quality top level.
    auto* badges = dq->addMenu(tr("&Badges"));

    auto* actBadgeDefs = badges->addAction(tr("Badge &Definitions"));
    connect(actBadgeDefs, &QAction::triggered, this, [this]() {
        if (badgeDefinitionController_)
            badgeDefinitionController_->showListWindow();
    });

    auto* actBadgeSevs = badges->addAction(tr("Badge &Severities"));
    connect(actBadgeSevs, &QAction::triggered, this, [this]() {
        if (badgeSeverityController_)
            badgeSeverityController_->showListWindow();
    });

    // Badge Mappings: browsable as a "Badge Mappings" tab on each Code
    // Domain's detail dialog (see BadgeMappingsTab), not a standalone
    // window of its own — this entry opens Code Domains, same as the
    // "Mappings" toolbar button on BadgeDefinitionMdiWindow.
    auto* actBadgeMappings = badges->addAction(tr("Badge &Mappings"));
    connect(actBadgeMappings, &QAction::triggered, this, [this]() {
        if (codeDomainController_)
            codeDomainController_->showListWindow();
    });

    // ---- Data Quality > Classifications ------------------------------------
    // Code Domains (this plugin), Coding Schemes, and Coding Scheme
    // Authority Types (moved in from data_management) — all
    // classification/coding lookups together.
    if (auto* classifications = smc.coding_schemes_menu) {
        auto* actCodeDomains = classifications->addAction(ico(Icon::Tag), tr("Code &Domains"));
        connect(actCodeDomains, &QAction::triggered, this, [this]() {
            if (codeDomainController_)
                codeDomainController_->showListWindow();
        });
        auto* actCodingSchemes = classifications->addAction(ico(Icon::Code), tr("Codin&g Schemes"));
        connect(actCodingSchemes, &QAction::triggered, this, [this]() {
            if (codingSchemeController_)
                codingSchemeController_->showListWindow();
        });
        auto* actCodingSchemeAuthorityTypes =
            classifications->addAction(ico(Icon::Tag), tr("Coding Scheme &Authority Types"));
        connect(actCodingSchemeAuthorityTypes, &QAction::triggered, this, [this]() {
            if (codingSchemeAuthorityTypeController_)
                codingSchemeAuthorityTypeController_->showListWindow();
        });
    }

    // lei_entity/lei_relationship/report_definition/synthetic_fx_spot_config
    // deliberately have no menu entry here: they are DQ-side staging views
    // with no Qt UI of their own (ores.cpp.qt disabled -- see each model's
    // "* Physical space" table). The two LEI entities have no working read
    // path other than the artefact table (which powers the GLEIF
    // counterparty picker, a different UI surface, not a same-component
    // browsing screen); report_definition/synthetic_fx_spot_config's
    // authoritative, editable home is ores.reporting/ores.synthetic
    // respectively.

    dq->addSeparator();

    // ---- Data Quality > Audit Trail (moved in from data_management) -------
    auto* menuAuditTrail = dq->addMenu(tr("&Audit Trail"));
    auto* actChangeReasonCategories =
        menuAuditTrail->addAction(ico(Icon::Tag), tr("Change Reason &Categories"));
    connect(actChangeReasonCategories, &QAction::triggered, this, [this]() {
        if (changeReasonCategoryController_)
            changeReasonCategoryController_->showListWindow();
    });
    auto* actChangeReasons = menuAuditTrail->addAction(ico(Icon::NoteEdit), tr("Change &Reasons"));
    connect(actChangeReasons, &QAction::triggered, this, [this]() {
        if (changeReasonController_)
            changeReasonController_->showListWindow();
    });

    dq->addSeparator();

    // ---- Data Quality > Data Catalogue (moved in from data_management) ----
    auto* menuCatalogue = dq->addMenu(tr("Data Ca&talogue"));

    auto* actArtefactTypes = menuCatalogue->addAction(ico(Icon::Table), tr("&Artefact Types"));
    connect(actArtefactTypes, &QAction::triggered, this, [this]() {
        if (artefactTypeController_)
            artefactTypeController_->showListWindow();
    });

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

    auto* actDatasets = menuCatalogue->addAction(ico(Icon::Folder), tr("&Datasets"));
    connect(actDatasets, &QAction::triggered, this, [this]() {
        if (datasetController_)
            datasetController_->showListWindow();
    });

    auto* actDatasetBundles = menuCatalogue->addAction(ico(Icon::Folder), tr("Dataset &Bundles"));
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

    // ---- Operations > Data Transfer > Data Librarian (moved in) -----------
    if (auto* dt = smc.data_transfer_menu) {
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
}

QList<QMenu*> DqPlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "Building plugin menus."
                               << " data_quality_menu=" << (data_quality_menu_ ? "ok" : "null");
    if (!data_quality_menu_) {
        BOOST_LOG_SEV(lg(), warn) << "Data Quality menu handle is missing — no menu will appear.";
        return {};
    }
    return {data_quality_menu_};
}

void DqPlugin::on_logout() {
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
    datasetController_.reset();
    codingSchemeController_.reset();
    codingSchemeAuthorityTypeController_.reset();
    changeReasonController_.reset();
    changeReasonCategoryController_.reset();
    artefactTypeController_.reset();
    codeDomainController_.reset();
    badgeSeverityController_.reset();
    badgeDefinitionController_.reset();
    ctx_ = {};
}

} // namespace ores::qt
