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
#include <QWizard>
#include <QWizardPage>
#include <QListWidget>
#include <QComboBox>
#include <QCheckBox>
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
 * @brief Wizard for publishing datasets from artefact tables to production.
 *
 * Multi-page wizard that guides users through:
 * 1. Review selected datasets
 * 2. Configure publication options
 * 3. Review publication order (with resolved dependencies)
 * 4. Monitor progress during publishing
 * 5. View results
 */
class PublishDatasetsDialog final : public QWizard {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.publish_datasets_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    // Page IDs
    enum PageId {
        Page_Selection,
        Page_Options,
        Page_Review,
        Page_Progress,
        Page_Results
    };

    explicit PublishDatasetsDialog(
        ClientManager* clientManager,
        const QString& username,
        QWidget* parent = nullptr);

    ~PublishDatasetsDialog() override = default;

    /**
     * @brief Set the datasets to publish.
     * @param datasets The selected datasets from the data librarian.
     */
    void setDatasets(const std::vector<dq::domain::dataset>& datasets);

    // Accessors for wizard pages
    ClientManager* clientManager() const { return clientManager_; }
    const QString& username() const { return username_; }
    const std::vector<dq::domain::dataset>& datasets() const { return datasets_; }
    std::vector<dq::domain::dataset>& resolvedDatasets() { return resolvedDatasets_; }
    std::vector<boost::uuids::uuid>& requestedIds() { return requestedIds_; }
    std::vector<dq::domain::publication_result>& results() { return results_; }

    // State accessors
    dq::domain::publication_mode selectedMode() const;
    bool resolveDependencies() const;

private:
    void setupPages();

    ClientManager* clientManager_;
    QString username_;
    std::vector<dq::domain::dataset> datasets_;  // Originally selected
    std::vector<dq::domain::dataset> resolvedDatasets_;  // Full list including deps
    std::vector<boost::uuids::uuid> requestedIds_;  // IDs explicitly requested
    std::vector<dq::domain::publication_result> results_;  // Publication results
};

// Forward declarations of page classes
class SelectionPage;
class OptionsPage;
class ReviewPage;
class ProgressPage;
class ResultsPage;

/**
 * @brief Page showing the selected datasets.
 */
class SelectionPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit SelectionPage(PublishDatasetsDialog* wizard);
    void initializePage() override;

private:
    PublishDatasetsDialog* wizard_;
    QListWidget* datasetList_;
    QLabel* countLabel_;
};

/**
 * @brief Page for configuring publication options.
 */
class OptionsPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit OptionsPage(PublishDatasetsDialog* wizard);

    QComboBox* modeCombo() const { return modeCombo_; }
    QCheckBox* resolveDependenciesCheck() const { return resolveDependenciesCheck_; }

private:
    PublishDatasetsDialog* wizard_;
    QComboBox* modeCombo_;
    QCheckBox* resolveDependenciesCheck_;
};

/**
 * @brief Page showing the resolved publication order.
 */
class ReviewPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit ReviewPage(PublishDatasetsDialog* wizard);
    void initializePage() override;
    bool isComplete() const override;

private:
    void resolveDependencies();
    void updatePublicationOrder();

    PublishDatasetsDialog* wizard_;
    QTableWidget* orderTable_;
    QLabel* summaryLabel_;
    QLabel* statusLabel_;
    bool resolved_ = false;
};

/**
 * @brief Page showing progress during publication.
 */
class ProgressPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit ProgressPage(PublishDatasetsDialog* wizard);
    void initializePage() override;
    bool isComplete() const override;
    int nextId() const override;

private:
    void performPublish();

    PublishDatasetsDialog* wizard_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    QLabel* currentDatasetLabel_;
    bool publishComplete_ = false;
    bool publishSuccess_ = false;
};

/**
 * @brief Page showing publication results.
 */
class ResultsPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit ResultsPage(PublishDatasetsDialog* wizard);
    void initializePage() override;

private:
    PublishDatasetsDialog* wizard_;
    QTableWidget* resultsTable_;
    QLabel* summaryLabel_;
};

}

#endif
