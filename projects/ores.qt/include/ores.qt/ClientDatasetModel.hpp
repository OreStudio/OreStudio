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
#ifndef ORES_QT_CLIENT_DATASET_MODEL_HPP
#define ORES_QT_CLIENT_DATASET_MODEL_HPP

#include <vector>
#include <QSize>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ColumnMetadata.hpp"
#include "ores.qt/RecencyPulseManager.hpp"
#include "ores.qt/RecencyTracker.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/dataset.hpp"

namespace ores::qt {

class ClientDatasetModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_dataset_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Column {
        Name,
        Code,
        Catalog,
        SubjectArea,
        Domain,
        Origin,
        Nature,
        Treatment,
        SourceSystem,
        AsOfDate,
        Version,
        ModifiedBy,
        RecordedAt,
        Tags,
        ColumnCount
    };

    // Custom roles for Tags column data
    static constexpr int OriginRole = Qt::UserRole + 100;
    static constexpr int NatureRole = Qt::UserRole + 101;
    static constexpr int TreatmentRole = Qt::UserRole + 102;

    /**
     * @brief Column metadata: header text, style, visibility, and width.
     *
     * Order must match the Column enum.
     */
    static constexpr std::size_t kColumnCount = std::size_t(ColumnCount);
    static constexpr std::array<ColumnMetadata, kColumnCount> kColumns = {{
        {
            .column = Name,
            .header = std::string_view("Name"),
            .style = column_style::text_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = Code,
            .header = std::string_view("Code"),
            .style = column_style::text_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = Catalog,
            .header = std::string_view("Catalog"),
            .style = column_style::text_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = SubjectArea,
            .header = std::string_view("Subject Area"),
            .style = column_style::text_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = Domain,
            .header = std::string_view("Domain"),
            .style = column_style::text_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = Origin,
            .header = std::string_view("Origin"),
            .style = column_style::text_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = Nature,
            .header = std::string_view("Nature"),
            .style = column_style::text_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = Treatment,
            .header = std::string_view("Treatment"),
            .style = column_style::text_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = SourceSystem,
            .header = std::string_view("Source System"),
            .style = column_style::text_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = AsOfDate,
            .header = std::string_view("As Of Date"),
            .style = column_style::mono_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = Version,
            .header = std::string_view("Version"),
            .style = column_style::mono_center,
            .hidden_by_default = false,
            .default_width = 70
        },
        {
            .column = ModifiedBy,
            .header = std::string_view("Modified By"),
            .style = column_style::text_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = RecordedAt,
            .header = std::string_view("Recorded At"),
            .style = column_style::mono_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = Tags,
            .header = std::string_view("Tags"),
            .style = column_style::text_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        }
    }};

    /**
     * @brief Default window size for the dataset list window.
     */
    inline static const QSize kDefaultWindowSize = {900, 400};

    /**
     * @brief Settings group name for persisting window and column state.
     */
    static constexpr std::string_view kSettingsGroup = "DatasetMdiWindow";
    /**
     * @brief Returns a static vector of column styles (built once per process).
     */
    static std::vector<column_style> const& columnStyles() {
        static std::vector<column_style> const kStylesVector = []() {
            std::vector<column_style> result;
            result.reserve(kColumnCount);
            for (std::size_t i = 0; i < kColumnCount; ++i)
                result.push_back(kColumns[i].style);
            return result;
        }();
        return kStylesVector;
    }

    /**
     * @brief Returns a static QVector of hidden column indices (built once per process).
     */
    static QVector<int> defaultHiddenColumns() {
        return ::ores::qt::defaultHiddenColumns<kColumnCount>(kColumns);
    }

    explicit ClientDatasetModel(ClientManager* clientManager,
                                QObject* parent = nullptr);
    ~ClientDatasetModel() override = default;

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    void refresh();
    const dq::domain::dataset* getDataset(int row) const;

signals:
    void dataLoaded();
    void loadError(const QString& error_message, const QString& details = {});

private slots:
    void onDatasetsLoaded();
    void onPulseStateChanged(bool isOn);
    void onPulsingComplete();

private:
    QVariant recency_foreground_color(const boost::uuids::uuid& id) const;

    struct FetchResult {
        bool success;
        std::vector<dq::domain::dataset> datasets;
        QString error_message;
        QString error_details;
    };

    ClientManager* clientManager_;
    std::vector<dq::domain::dataset> datasets_;
    QFutureWatcher<FetchResult>* watcher_;
    bool is_fetching_{false};

    using DatasetKeyExtractor = std::string(*)(const dq::domain::dataset&);
    RecencyTracker<dq::domain::dataset, DatasetKeyExtractor> recencyTracker_;
    RecencyPulseManager* pulseManager_;
};

}

#endif
