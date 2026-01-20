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
#ifndef ORES_QT_PUBLISH_DATASETS_DIALOG_HPP
#define ORES_QT_PUBLISH_DATASETS_DIALOG_HPP

#include <vector>
#include <QDialog>
#include <QListWidget>
#include <QComboBox>
#include <QCheckBox>
#include <QPushButton>
#include <QTableWidget>
#include <QLabel>
#include <QProgressBar>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/dataset.hpp"
#include "ores.dq/domain/publication_mode.hpp"
#include "ores.dq/domain/publication_result.hpp"

namespace ores::qt {

class ClientManager;

/**
 * @brief Dialog for publishing datasets from artefact tables to production.
 *
 * Allows users to select publication mode and configure dependency resolution
 * before publishing datasets. Shows results with record counts after completion.
 */
class PublishDatasetsDialog final : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.publish_datasets_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PublishDatasetsDialog(
        ClientManager* clientManager,
        const QString& username,
        QWidget* parent = nullptr);

    ~PublishDatasetsDialog() override = default;

    /**
     * @brief Set the datasets to publish.
     *
     * @param datasets The selected datasets from the data librarian.
     */
    void setDatasets(const std::vector<dq::domain::dataset>& datasets);

private slots:
    void onPublishClicked();
    void onCloseClicked();

private:
    void setupUi();
    void setupConnections();
    void showResults(const std::vector<dq::domain::publication_result>& results);
    void setUiEnabled(bool enabled);

    ClientManager* clientManager_;
    QString username_;
    std::vector<dq::domain::dataset> datasets_;

    // Dataset list
    QListWidget* datasetList_;
    QLabel* countLabel_;

    // Options
    QComboBox* modeCombo_;
    QCheckBox* resolveDependenciesCheck_;

    // Buttons
    QPushButton* publishButton_;
    QPushButton* closeButton_;

    // Results
    QTableWidget* resultsTable_;
    QLabel* summaryLabel_;
    QProgressBar* progressBar_;
};

}

#endif
