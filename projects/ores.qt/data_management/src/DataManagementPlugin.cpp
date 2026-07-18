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
#include "ores.qt/DataManagementPlugin.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/CatalogController.hpp"
#include "ores.qt/ChangeReasonCategoryController.hpp"
#include "ores.qt/ChangeReasonController.hpp"
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
    static auto instance = make_logger("ores.qt.data_management_plugin");
    return instance;
}

}

DataManagementPlugin::DataManagementPlugin(QObject* parent)
    : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

DataManagementPlugin::~DataManagementPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

void DataManagementPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

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

void DataManagementPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Registering entries in shared menus."
                               << " data_quality=" << (smc.data_quality_menu ? "ok" : "null")
                               << " operations=" << (smc.operations_menu ? "ok" : "null")
                               << " data_transfer=" << (smc.data_transfer_menu ? "ok" : "null");

    using IC = IconUtils;
    auto ico = [](Icon i) {
        return IC::createRecoloredIcon(i, IC::DefaultIconColor);
    };

    // ---- Data Quality > Classifications / Audit Trail ---------------------
    // The Data Management menu is retired; these two submenus (and their
    // controllers, unchanged) move to Data Quality alongside DqPlugin's own
    // Badges/Code Domains — all auxiliary classification/lookup data, not
    // primary entities.
    if (auto* dq = smc.data_quality_menu) {
        dq->addSeparator();

        auto* menuClassifications = dq->addMenu(tr("C&lassifications"));
        auto* actCodingSchemes =
            menuClassifications->addAction(ico(Icon::Code), tr("Codin&g Schemes"));
        connect(actCodingSchemes, &QAction::triggered, this, [this]() {
            if (codingSchemeController_)
                codingSchemeController_->showListWindow();
        });
        auto* actCodingSchemeAuthorityTypes =
            menuClassifications->addAction(ico(Icon::Tag), tr("Coding Scheme &Authority Types"));
        connect(actCodingSchemeAuthorityTypes, &QAction::triggered, this, [this]() {
            if (codingSchemeAuthorityTypeController_)
                codingSchemeAuthorityTypeController_->showListWindow();
        });

        auto* menuAuditTrail = dq->addMenu(tr("&Audit Trail"));
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

    // ---- Operations > Data Transfer ---------------------------------------
    // Import/transfer-shaped actions (Data Catalogue, Data Librarian; also
    // contributed to by TradingPlugin and WorkspacePlugin) move to Operations
    // under a shared Data Transfer submenu this plugin owns attaching.
    auto* dt = smc.data_transfer_menu;
    if (smc.operations_menu && dt)
        smc.operations_menu->addMenu(dt);
    if (!dt)
        return;

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

    act_data_librarian_ =
        dt->addAction(IconUtils::createRecoloredIcon(Icon::Library, IconUtils::DefaultIconColor),
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
        connect(
            subWindow, &QObject::destroyed, this, [this]() { data_librarian_window_ = nullptr; });

        ctx_.mdi_area->addSubWindow(subWindow);
        subWindow->resize(librarianWindow->sizeHint());
        subWindow->show();
    });
}

QList<QMenu*> DataManagementPlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug)
        << "No standalone menu — all entries contributed to Data Quality/Operations via "
           "setup_menus().";
    return {};
}

void DataManagementPlugin::on_logout() {
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
    ctx_ = {};
}

} // namespace ores::qt
