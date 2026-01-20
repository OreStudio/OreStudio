/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_CLIENT_CATALOG_DEPENDENCY_MODEL_HPP
#define ORES_QT_CLIENT_CATALOG_DEPENDENCY_MODEL_HPP

#include <QAbstractTableModel>
#include <vector>
#include "ores.dq/domain/catalog_dependency.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Table model for displaying catalog dependencies.
 *
 * Provides async loading from server. This is a read-only model
 * for viewing dependencies between catalogs.
 */
class ClientCatalogDependencyModel : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_catalog_dependency_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Column {
        CatalogName = 0,
        DependencyName,
        RecordedBy,
        RecordedAt,
        ColumnCount
    };

    explicit ClientCatalogDependencyModel(ClientManager* clientManager,
                                          QObject* parent = nullptr);

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index,
                  int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
                        int role = Qt::DisplayRole) const override;

    void loadData();
    void loadDataByCatalog(const std::string& catalog_name);

    const std::vector<dq::domain::catalog_dependency>& dependencies() const;

    /**
     * @brief Gets dependencies for a specific catalog from the loaded data.
     *
     * @param catalog_name The catalog to get dependencies for
     * @return Dependencies where this catalog is the dependent (has dependencies on others)
     */
    std::vector<dq::domain::catalog_dependency>
    dependenciesForCatalog(const std::string& catalog_name) const;

signals:
    void loadStarted();
    void loadFinished();
    void errorOccurred(const QString& message, const QString& details = {});

private:
    ClientManager* clientManager_;
    std::vector<dq::domain::catalog_dependency> dependencies_;
};

}

#endif
