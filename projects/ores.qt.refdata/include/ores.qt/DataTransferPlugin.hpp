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
#ifndef ORES_QT_DATA_TRANSFER_PLUGIN_HPP
#define ORES_QT_DATA_TRANSFER_PLUGIN_HPP

#include <memory>
#include <QList>
#include "ores.qt/PluginBase.hpp"

namespace ores::qt {

class DataDomainController;
class SubjectAreaController;
class CatalogController;
class DatasetBundleController;
class MethodologyController;
class OriginDimensionController;
class NatureDimensionController;
class TreatmentDimensionController;

/**
 * @brief Plugin owning the Data Transfer domain: data catalogue and import tools.
 *
 * Provides the top-level Data Transfer menu. Data Librarian and Import ORE Data
 * are contributed by RefdataPlugin and TradingPlugin respectively via setup_menus.
 * Loaded as a shared library by QPluginLoader at application startup.
 */
class DataTransferPlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit DataTransferPlugin(QObject* parent = nullptr);
    ~DataTransferPlugin() override;

    QString name() const override { return QStringLiteral("ores.qt.data_transfer"); }
    int load_order() const override { return 380; }

    void on_login(const plugin_context& ctx) override;
    void setup_menus(const shared_menus_context& ctx) override;
    QList<QMenu*> create_menus() override;
    void on_logout() override;

private:
    plugin_context ctx_;

    // The data_transfer_menu is pre-created by MainWindow and passed via
    // setup_menus context. We hold a reference to return it from create_menus.
    QMenu* data_transfer_menu_{nullptr};

    std::unique_ptr<DataDomainController>         dataDomainController_;
    std::unique_ptr<SubjectAreaController>        subjectAreaController_;
    std::unique_ptr<CatalogController>            catalogController_;
    std::unique_ptr<DatasetBundleController>      datasetBundleController_;
    std::unique_ptr<MethodologyController>        methodologyController_;
    std::unique_ptr<OriginDimensionController>    originDimensionController_;
    std::unique_ptr<NatureDimensionController>    natureDimensionController_;
    std::unique_ptr<TreatmentDimensionController> treatmentDimensionController_;
};

}

#endif
