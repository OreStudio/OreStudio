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

#include "ores.dq.api/domain/dataset.hpp"
#include "ores.dq.api/domain/publication_mode.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/WorkflowStepsWidget.hpp"
#include <QCheckBox>
#include <QComboBox>
#include <QLabel>
#include <QListWidget>
#include <QProgressBar>
#include <QTableWidget>
#include <QWizard>
#include <QWizardPage>
#include <boost/uuid/uuid.hpp>
#include <vector>

namespace ores::qt {

class ClientManager;

/**
 * @brief Wizard for publishing datasets from artefact tables to production.
 *
 * Multi-page wizard that guides users through:
 * 1. Review selected datasets
 * 2. Configure publication options
 * 3. Review publication order (with resolved dependencies)
 * 4. Monitor NATS request progress
 * 5. Watch per-step workflow progress via WorkflowStepsWidget
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
signals:
    /**
     * @brief Emitted when the publication workflow completes successfully.
     */
    void datasetsPublished(const QStringList& datasetCodes);

public:
    enum PageId { Page_Selection, Page_Options, Page_Review, Page_Progress, Page_Results };

    explicit PublishDatasetsDialog(ClientManager* clientManager,
                                   const QString& username,
                                   QWidget* parent = nullptr);

    ~PublishDatasetsDialog() override = default;

    void setDatasets(const std::vector<dq::domain::dataset>& datasets);

    // Accessors for wizard pages
    ClientManager* clientManager() const {
        return clientManager_;
    }
    const QString& username() const {
        return username_;
    }
    const std::vector<dq::domain::dataset>& datasets() const {
        return datasets_;
    }
    std::vector<dq::domain::dataset>& resolvedDatasets() {
        return resolvedDatasets_;
    }
    std::vector<std::string>& requestedIds() {
        return requestedIds_;
    }
    std::string& instanceId() {
        return instanceId_;
    }
    int& datasetsDispatched() {
        return datasetsDispatched_;
    }

    dq::domain::publication_mode selectedMode() const;
    bool resolveDependencies() const;

private:
    void setupPages();

    ClientManager* clientManager_;
    QString username_;
    std::vector<dq::domain::dataset> datasets_;
    std::vector<dq::domain::dataset> resolvedDatasets_;
    std::vector<std::string> requestedIds_;
    std::string instanceId_;
    int datasetsDispatched_ = 0;
};

class SelectionPage;
class OptionsPage;
class ReviewPage;
class ProgressPage;
class ResultsPage;

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

class OptionsPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit OptionsPage(PublishDatasetsDialog* wizard);

    QComboBox* modeCombo() const {
        return modeCombo_;
    }
    QCheckBox* resolveDependenciesCheck() const {
        return resolveDependenciesCheck_;
    }

private:
    PublishDatasetsDialog* wizard_;
    QComboBox* modeCombo_;
    QCheckBox* resolveDependenciesCheck_;
};

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
    bool publishComplete_ = false;
    bool publishSuccess_ = false;
};

class ResultsPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit ResultsPage(PublishDatasetsDialog* wizard);
    void setResults(bool success, const QString& errorMessage);
    void initializePage() override;
    bool isComplete() const override;

private slots:
    void onWorkflowComplete(bool success);

private:
    PublishDatasetsDialog* wizard_;
    QLabel* overallStatusLabel_;
    WorkflowStepsWidget* stepsWidget_;
    bool overallSuccess_ = false;
    QString errorMessage_;
    bool workflowComplete_ = false;
};

}

#endif
