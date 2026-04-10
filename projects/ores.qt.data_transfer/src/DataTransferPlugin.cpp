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
#include "ores.qt/DataTransferPlugin.hpp"

#include <QMenu>
#include <QAction>

#include "ores.qt/IconUtils.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/DataDomainController.hpp"
#include "ores.qt/SubjectAreaController.hpp"
#include "ores.qt/CatalogController.hpp"
#include "ores.qt/DatasetBundleController.hpp"
#include "ores.qt/MethodologyController.hpp"
#include "ores.qt/OriginDimensionController.hpp"
#include "ores.qt/NatureDimensionController.hpp"
#include "ores.qt/TreatmentDimensionController.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.qt.data_transfer_plugin");
    return instance;
}

auto ico(Icon icon) {
    return IconUtils::createRecoloredIcon(icon, IconUtils::DefaultIconColor);
}

}

DataTransferPlugin::DataTransferPlugin(QObject* parent) : PluginBase(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Plugin initialised.";
}

DataTransferPlugin::~DataTransferPlugin() {
    BOOST_LOG_SEV(lg(), debug) << "Plugin shutdown.";
}

void DataTransferPlugin::on_login(const plugin_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Login event received.";
    ctx_ = ctx;

    dataDomainController_ = std::make_unique<DataDomainController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(dataDomainController_.get());

    subjectAreaController_ = std::make_unique<SubjectAreaController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(subjectAreaController_.get());

    catalogController_ = std::make_unique<CatalogController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(catalogController_.get());

    datasetBundleController_ = std::make_unique<DatasetBundleController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(datasetBundleController_.get());

    methodologyController_ = std::make_unique<MethodologyController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(methodologyController_.get());

    originDimensionController_ = std::make_unique<OriginDimensionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(originDimensionController_.get());

    natureDimensionController_ = std::make_unique<NatureDimensionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(natureDimensionController_.get());

    treatmentDimensionController_ = std::make_unique<TreatmentDimensionController>(
        ctx_.main_window, ctx_.mdi_area, ctx_.client_manager,
        ctx_.change_reason_cache, ctx_.username, this);
    connectControllerSignals(treatmentDimensionController_.get());
}

void DataTransferPlugin::setup_menus(const shared_menus_context& smc) {
    BOOST_LOG_SEV(lg(), debug) << "Capturing shared Data Transfer menu handle."
        << " data_transfer=" << (smc.data_transfer_menu ? "ok" : "null");
    // Save reference so create_menus() can return the pre-created menu.
    // Other plugins (RefdataPlugin, TradingPlugin) contribute actions to it
    // during their own setup_menus calls.
    data_transfer_menu_ = smc.data_transfer_menu;
}

QList<QMenu*> DataTransferPlugin::create_menus() {
    BOOST_LOG_SEV(lg(), debug) << "Building plugin menus."
        << " data_transfer_menu=" << (data_transfer_menu_ ? "ok" : "null");
    if (!data_transfer_menu_) {
        BOOST_LOG_SEV(lg(), warn) << "Data Transfer menu handle is missing — no menu will appear.";
        return {};
    }

    // ---- Data Catalogue submenu -----------------------------------------
    auto* menuCatalogue = data_transfer_menu_->addMenu(tr("Data Ca&talogue"));

    auto* actDataDomains = menuCatalogue->addAction(ico(Icon::Folder), tr("&Data Domains"));
    connect(actDataDomains, &QAction::triggered, this, [this]() {
        if (dataDomainController_) dataDomainController_->showListWindow();
    });

    auto* actSubjectAreas = menuCatalogue->addAction(ico(Icon::Table), tr("&Subject Areas"));
    connect(actSubjectAreas, &QAction::triggered, this, [this]() {
        if (subjectAreaController_) subjectAreaController_->showListWindow();
    });

    auto* actCatalogs = menuCatalogue->addAction(ico(Icon::Library), tr("&Catalogues"));
    connect(actCatalogs, &QAction::triggered, this, [this]() {
        if (catalogController_) catalogController_->showListWindow();
    });

    auto* actDatasetBundles = menuCatalogue->addAction(
        ico(Icon::Folder), tr("Dataset &Bundles"));
    connect(actDatasetBundles, &QAction::triggered, this, [this]() {
        if (datasetBundleController_) datasetBundleController_->showListWindow();
    });

    auto* actMethodologies = menuCatalogue->addAction(ico(Icon::Book), tr("&Methodologies"));
    connect(actMethodologies, &QAction::triggered, this, [this]() {
        if (methodologyController_) methodologyController_->showListWindow();
    });

    menuCatalogue->addSeparator();

    auto* actOriginDimensions = menuCatalogue->addAction(
        ico(Icon::Database), tr("&Origin Dimensions"));
    connect(actOriginDimensions, &QAction::triggered, this, [this]() {
        if (originDimensionController_) originDimensionController_->showListWindow();
    });

    auto* actNatureDimensions = menuCatalogue->addAction(
        ico(Icon::Database), tr("&Nature Dimensions"));
    connect(actNatureDimensions, &QAction::triggered, this, [this]() {
        if (natureDimensionController_) natureDimensionController_->showListWindow();
    });

    auto* actTreatmentDimensions = menuCatalogue->addAction(
        ico(Icon::Database), tr("&Treatment Dimensions"));
    connect(actTreatmentDimensions, &QAction::triggered, this, [this]() {
        if (treatmentDimensionController_) treatmentDimensionController_->showListWindow();
    });

    BOOST_LOG_SEV(lg(), debug) << "Plugin menus ready.";
    return {data_transfer_menu_};
}

void DataTransferPlugin::on_logout() {
    BOOST_LOG_SEV(lg(), debug) << "Logout event received.";
    treatmentDimensionController_.reset();
    natureDimensionController_.reset();
    originDimensionController_.reset();
    methodologyController_.reset();
    datasetBundleController_.reset();
    catalogController_.reset();
    subjectAreaController_.reset();
    dataDomainController_.reset();
    ctx_ = {};
}

} // namespace ores::qt
