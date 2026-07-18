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
#ifndef ORES_QT_DATA_MANAGEMENT_PLUGIN_HPP
#define ORES_QT_DATA_MANAGEMENT_PLUGIN_HPP

#include "ores.qt/PluginBase.hpp"
#include <QList>
#include <memory>

class QAction;

namespace ores::qt {

class DetachableMdiSubWindow;
class CatalogController;
class ChangeReasonController;
class ChangeReasonCategoryController;
class CodingSchemeController;
class CodingSchemeAuthorityTypeController;
class DataDomainController;
class DatasetController;
class DatasetBundleController;
class MethodologyController;
class NatureDimensionController;
class OriginDimensionController;
class SubjectAreaController;
class TreatmentDimensionController;

/**
 * @brief Qt plugin owning the ores.dq-backed entities' controllers (Data
 * Catalogue, Change Reason, Coding Scheme, Data Librarian).
 *
 * No standalone top-level menu of its own (the former Data Management
 * menu is retired) — contributes Classifications/Audit Trail to the
 * shared data_quality_menu, and Data Catalogue/Data Librarian to the
 * shared data_transfer_menu (which it also attaches to operations_menu;
 * also contributed to by TradingPlugin and WorkspacePlugin) via
 * setup_menus.
 */
class DataManagementPlugin : public PluginBase {
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "ores.qt.IPlugin/1.0")
    Q_INTERFACES(ores::qt::IPlugin)

public:
    explicit DataManagementPlugin(QObject* parent = nullptr);
    ~DataManagementPlugin() override;

    QString name() const override {
        return QStringLiteral("ores.qt.data_management");
    }
    int load_order() const override {
        return 375;
    }

    void on_login(const plugin_context& ctx) override;
    void setup_menus(const shared_menus_context& ctx) override;
    QList<QMenu*> create_menus() override;
    void on_logout() override;

private:
    plugin_context ctx_;

    QAction* act_data_librarian_{nullptr};

    // Singleton MDI sub-window for Data Librarian (nullptr when not open)
    DetachableMdiSubWindow* data_librarian_window_{nullptr};

    std::unique_ptr<ChangeReasonCategoryController> changeReasonCategoryController_;
    std::unique_ptr<ChangeReasonController> changeReasonController_;
    std::unique_ptr<CodingSchemeAuthorityTypeController> codingSchemeAuthorityTypeController_;
    std::unique_ptr<CodingSchemeController> codingSchemeController_;
    std::unique_ptr<DatasetController> datasetController_;

    // Data Catalogue controllers
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
