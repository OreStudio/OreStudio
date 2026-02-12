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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_PUBLISH_BUNDLE_WIZARD_HPP
#define ORES_QT_PUBLISH_BUNDLE_WIZARD_HPP

#include <vector>
#include <QWizard>
#include <QWizardPage>
#include <QLabel>
#include <QComboBox>
#include <QCheckBox>
#include <QProgressBar>
#include <QTableView>
#include <QStandardItemModel>
#include <QSet>
#include <QVBoxLayout>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.dq/domain/dataset_bundle_member.hpp"
#include "ores.dq/domain/publication_mode.hpp"
#include "ores.dq/messaging/publish_bundle_protocol.hpp"

namespace ores::qt {

class LeiEntityPicker;

/**
 * @brief Wizard for publishing a dataset bundle.
 *
 * Multi-page wizard that guides users through:
 * 1. Reviewing bundle contents (datasets and their order)
 * 2. Optionally configuring LEI entity parameters (if bundle contains
 *    lei_parties datasets)
 * 3. Confirming publication settings (mode, atomicity)
 * 4. Executing the publication and showing progress
 * 5. Displaying per-dataset results
 */
class PublishBundleWizard final : public QWizard {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.publish_bundle_wizard";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum PageId {
        Page_BundleSummary,
        Page_OptionalDatasets,
        Page_LeiPartyConfig,
        Page_Confirm,
        Page_Progress,
        Page_Results
    };

    explicit PublishBundleWizard(
        ClientManager* clientManager,
        const QString& bundleCode,
        const QString& bundleName,
        QWidget* parent = nullptr);

    ~PublishBundleWizard() override = default;

    ClientManager* clientManager() const { return clientManager_; }
    QString bundleCode() const { return bundleCode_; }
    QString bundleName() const { return bundleName_; }

    bool needsLeiPartyConfig() const { return needsLeiPartyConfig_; }
    void setNeedsLeiPartyConfig(bool needs) { needsLeiPartyConfig_ = needs; }

    QString rootLei() const { return rootLei_; }
    void setRootLei(const QString& lei) { rootLei_ = lei; }

    QString rootLeiName() const { return rootLeiName_; }
    void setRootLeiName(const QString& name) { rootLeiName_ = name; }

    QString leiDatasetSize() const { return leiDatasetSize_; }
    void setLeiDatasetSize(const QString& size) { leiDatasetSize_ = size; }

    bool hasOptionalDatasets() const { return hasOptionalDatasets_; }
    void setHasOptionalDatasets(bool has) { hasOptionalDatasets_ = has; }

    const QSet<QString>& optedInDatasets() const { return optedInDatasets_; }
    void setOptedInDatasets(const QSet<QString>& datasets) {
        optedInDatasets_ = datasets;
    }

    /**
     * @brief Get the loaded bundle members.
     */
    const std::vector<dq::domain::dataset_bundle_member>& members() const {
        return members_;
    }

    /**
     * @brief Set the loaded bundle members.
     */
    void setMembers(std::vector<dq::domain::dataset_bundle_member> members) {
        members_ = std::move(members);
    }

    /**
     * @brief Get the selected publication mode.
     */
    dq::domain::publication_mode selectedMode() const { return selectedMode_; }
    void setSelectedMode(dq::domain::publication_mode mode) {
        selectedMode_ = mode;
    }

    /**
     * @brief Get the atomic publication flag.
     */
    bool isAtomic() const { return atomic_; }
    void setAtomic(bool atomic) { atomic_ = atomic; }

signals:
    /**
     * @brief Emitted when the bundle has been published successfully.
     *
     * @param bundleCode The code of the published bundle.
     */
    void bundlePublished(const QString& bundleCode);

private:
    void setupPages();

    ClientManager* clientManager_;
    QString bundleCode_;
    QString bundleName_;
    bool needsLeiPartyConfig_ = false;
    bool hasOptionalDatasets_ = false;
    QSet<QString> optedInDatasets_;
    QString rootLei_;
    QString rootLeiName_;
    QString leiDatasetSize_ = QStringLiteral("large");
    std::vector<dq::domain::dataset_bundle_member> members_;
    dq::domain::publication_mode selectedMode_ =
        dq::domain::publication_mode::upsert;
    bool atomic_ = true;
};

// Forward declarations of page classes
class BundleSummaryPage;
class OptionalDatasetsPage;
class LeiPartyConfigPage;
class ConfirmPublishPage;
class PublishProgressPage;
class PublishResultsPage;

/**
 * @brief Page showing bundle contents and dataset list.
 *
 * Loads bundle members from the server and displays them in a table.
 * Detects whether any dataset requires LEI party configuration.
 */
class BundleSummaryPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.bundle_summary_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit BundleSummaryPage(PublishBundleWizard* wizard);
    void initializePage() override;
    int nextId() const override;

private:
    void setupUI();
    void loadMembers();
    void populateTable();

    PublishBundleWizard* wizard_;
    QLabel* bundleNameLabel_;
    QTableView* membersTable_;
    QStandardItemModel* membersModel_;
    QLabel* statusLabel_;
    bool membersLoaded_ = false;
};

/**
 * @brief Page for opting in to optional datasets.
 *
 * Shown when the bundle contains optional datasets (e.g., LEI parties,
 * counterparties). Each optional dataset is presented as a checkbox.
 * Counterparty datasets are disabled with a tooltip explaining they
 * require a future migration.
 */
class OptionalDatasetsPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit OptionalDatasetsPage(PublishBundleWizard* wizard);
    void initializePage() override;
    bool validatePage() override;
    int nextId() const override;

private:
    void setupUI();

    PublishBundleWizard* wizard_;
    QVBoxLayout* checkboxLayout_;
    std::vector<QCheckBox*> checkboxes_;
};

/**
 * @brief Page for configuring LEI entity parameters.
 *
 * Only shown when the bundle contains a lei_parties dataset. Embeds
 * a LeiEntityPicker widget for selecting the root LEI entity.
 */
class LeiPartyConfigPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit LeiPartyConfigPage(PublishBundleWizard* wizard);
    void initializePage() override;
    bool validatePage() override;
    int nextId() const override;

private:
    void setupUI();

    PublishBundleWizard* wizard_;
    LeiEntityPicker* leiPicker_;
    QLabel* instructionLabel_;
    QLabel* selectedEntityLabel_;
    bool leiLoaded_ = false;
};

/**
 * @brief Confirmation page showing publication summary.
 *
 * Displays what will be published, including bundle name, dataset count,
 * LEI configuration (if applicable), and publication settings.
 */
class ConfirmPublishPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit ConfirmPublishPage(PublishBundleWizard* wizard);
    void initializePage() override;
    bool validatePage() override;
    int nextId() const override;

private:
    void setupUI();

    PublishBundleWizard* wizard_;
    QLabel* summaryLabel_;
    QComboBox* modeCombo_;
    QCheckBox* atomicCheckbox_;
};

/**
 * @brief Progress page that executes the publication.
 *
 * Sends the publish_bundle_request to the server asynchronously
 * and shows an indeterminate progress bar during the operation.
 */
class PublishProgressPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.publish_progress_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PublishProgressPage(PublishBundleWizard* wizard);
    void initializePage() override;
    bool isComplete() const override;
    int nextId() const override;

private:
    void startPublish();

    PublishBundleWizard* wizard_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    bool publishComplete_ = false;
    bool publishSuccess_ = false;
};

/**
 * @brief Results page showing per-dataset publication outcomes.
 *
 * Displays a table of results for each dataset in the bundle,
 * including record counts for inserts, updates, skips, deletes,
 * and any error messages.
 */
class PublishResultsPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit PublishResultsPage(PublishBundleWizard* wizard);
    void initializePage() override;

    /**
     * @brief Set results data for display.
     *
     * Called by PublishProgressPage when the publication completes.
     */
    void setResults(bool overallSuccess,
                    const QString& errorMessage,
                    const std::vector<dq::messaging::bundle_dataset_result>& results);

private:
    void setupUI();
    void populateResults();

    PublishBundleWizard* wizard_;
    QLabel* overallStatusLabel_;
    QTableView* resultsTable_;
    QStandardItemModel* resultsModel_;

    bool overallSuccess_ = false;
    QString errorMessage_;
    std::vector<dq::messaging::bundle_dataset_result> datasetResults_;
};

}

#endif
