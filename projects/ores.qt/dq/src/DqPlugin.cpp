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
#include "ores.qt/BadgeDefinitionController.hpp"
#include "ores.qt/BadgeSeverityController.hpp"
#include "ores.qt/CatalogController.hpp"
#include "ores.qt/ChangeReasonCategoryController.hpp"
#include "ores.qt/ChangeReasonController.hpp"
#include "ores.qt/CodeDomainController.hpp"
#include "ores.qt/DataDomainController.hpp"
#include "ores.qt/DatasetBundleController.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LeiEntityController.hpp"
#include "ores.qt/LeiRelationshipController.hpp"
#include <QAction>
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

    catalogController_ = std::make_unique<CatalogController>(ctx_.main_window,
                                                              ctx_.mdi_area,
                                                              ctx_.client_manager,
                                                              ctx_.change_reason_cache,
                                                              ctx_.username,
                                                              this);
    connectControllerSignals(catalogController_.get());

    changeReasonCategoryController_ =
        std::make_unique<ChangeReasonCategoryController>(ctx_.main_window,
                                                          ctx_.mdi_area,
                                                          ctx_.client_manager,
                                                          ctx_.change_reason_cache,
                                                          ctx_.username,
                                                          this);
    connectControllerSignals(changeReasonCategoryController_.get());

    changeReasonController_ =
        std::make_unique<ChangeReasonController>(ctx_.main_window,
                                                  ctx_.mdi_area,
                                                  ctx_.client_manager,
                                                  ctx_.change_reason_cache,
                                                  ctx_.username,
                                                  this);
    connectControllerSignals(changeReasonController_.get());

    dataDomainController_ = std::make_unique<DataDomainController>(ctx_.main_window,
                                                                    ctx_.mdi_area,
                                                                    ctx_.client_manager,
                                                                    ctx_.change_reason_cache,
                                                                    ctx_.username,
                                                                    this);
    connectControllerSignals(dataDomainController_.get());

    datasetBundleController_ =
        std::make_unique<DatasetBundleController>(ctx_.main_window,
                                                   ctx_.mdi_area,
                                                   ctx_.client_manager,
                                                   ctx_.change_reason_cache,
                                                   ctx_.username,
                                                   this);
    connectControllerSignals(datasetBundleController_.get());

    leiEntityController_ = std::make_unique<LeiEntityController>(ctx_.main_window,
                                                                  ctx_.mdi_area,
                                                                  ctx_.client_manager,
                                                                  ctx_.change_reason_cache,
                                                                  ctx_.username,
                                                                  this);
    connectControllerSignals(leiEntityController_.get());

    leiRelationshipController_ =
        std::make_unique<LeiRelationshipController>(ctx_.main_window,
                                                     ctx_.mdi_area,
                                                     ctx_.client_manager,
                                                     ctx_.change_reason_cache,
                                                     ctx_.username,
                                                     this);
    connectControllerSignals(leiRelationshipController_.get());

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
}

void DqPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Capturing shared Data Quality menu handle."
                               << " data_quality=" << (smc.data_quality_menu ? "ok" : "null")
                               << " classifications=" << (smc.coding_schemes_menu ? "ok" : "null");
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

    // Code Domains lives in the shared Classifications submenu alongside
    // Coding Schemes and Coding Scheme Authority Types (contributed by
    // DataManagementPlugin) — all classification/coding lookups together.
    if (auto* classifications = smc.coding_schemes_menu) {
        auto* actCodeDomains = classifications->addAction(ico(Icon::Tag), tr("Code &Domains"));
        connect(actCodeDomains, &QAction::triggered, this, [this]() {
            if (codeDomainController_)
                codeDomainController_->showListWindow();
        });
    }

    // &Governance submenu: foundational DQ lookups that every other entity
    // in the system leans on (catalog ownership, change-reason taxonomy,
    // data domains) -- distinct from Badges (severity/classification) and
    // Classifications (coding schemes/code domains) above.
    auto* governance = dq->addMenu(tr("&Governance"));

    auto* actCatalogs = governance->addAction(tr("&Catalogs"));
    connect(actCatalogs, &QAction::triggered, this, [this]() {
        if (catalogController_)
            catalogController_->showListWindow();
    });

    auto* actDataDomains = governance->addAction(tr("&Data Domains"));
    connect(actDataDomains, &QAction::triggered, this, [this]() {
        if (dataDomainController_)
            dataDomainController_->showListWindow();
    });

    auto* actChangeReasons = governance->addAction(tr("Change &Reasons"));
    connect(actChangeReasons, &QAction::triggered, this, [this]() {
        if (changeReasonController_)
            changeReasonController_->showListWindow();
    });

    auto* actChangeReasonCategories = governance->addAction(tr("Change Reason &Categories"));
    connect(actChangeReasonCategories, &QAction::triggered, this, [this]() {
        if (changeReasonCategoryController_)
            changeReasonCategoryController_->showListWindow();
    });

    // Dataset Bundles: top-level entry point, same tier as Badges -- it is
    // the primary DQ management concept for grouping installable datasets.
    auto* actDatasetBundles = dq->addAction(tr("Dataset &Bundles"));
    connect(actDatasetBundles, &QAction::triggered, this, [this]() {
        if (datasetBundleController_)
            datasetBundleController_->showListWindow();
    });

    // &LEI Registry submenu: the two GLEIF LEI entities (entity + corporate
    // hierarchy relationship) belong together as a single registry concept,
    // distinct from the generic badge/coding-scheme governance above.
    auto* leiRegistry = dq->addMenu(tr("&LEI Registry"));

    auto* actLeiEntities = leiRegistry->addAction(tr("LEI &Entities"));
    connect(actLeiEntities, &QAction::triggered, this, [this]() {
        if (leiEntityController_)
            leiEntityController_->showListWindow();
    });

    auto* actLeiRelationships = leiRegistry->addAction(tr("LEI &Relationships"));
    connect(actLeiRelationships, &QAction::triggered, this, [this]() {
        if (leiRelationshipController_)
            leiRelationshipController_->showListWindow();
    });

    // report_definition/synthetic_fx_spot_config deliberately have no menu
    // entry here: they are DQ-side staging views with no Qt UI of their own
    // (ores.cpp.qt disabled -- see each model's "* Physical space" table).
    // Their authoritative, editable home is ores.reporting/ores.synthetic
    // respectively.
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
    leiRelationshipController_.reset();
    leiEntityController_.reset();
    datasetBundleController_.reset();
    dataDomainController_.reset();
    changeReasonController_.reset();
    changeReasonCategoryController_.reset();
    catalogController_.reset();
    codeDomainController_.reset();
    badgeSeverityController_.reset();
    badgeDefinitionController_.reset();
    ctx_ = {};
}

} // namespace ores::qt
