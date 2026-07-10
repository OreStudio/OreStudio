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
#ifndef ORES_QT_PARTY_PROVISIONING_WIZARD_HPP
#define ORES_QT_PARTY_PROVISIONING_WIZARD_HPP

#include "ores.dq.api/messaging/report_definition_template_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/WorkflowStepsWidget.hpp"
#include <QComboBox>
#include <QLabel>
#include <QListWidget>
#include <QProgressBar>
#include <QTextEdit>
#include <QWizard>
#include <QWizardPage>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::qt {

/**
 * @brief Wizard for setting up a party after it has been created by tenant provisioning.
 *
 * Assumes the party hierarchy already exists.
 *
 * Collect phase (zero backend writes):
 * 1. Welcome             - explains the setup process
 * 2. Counterparty Setup  - select dataset size for GLEIF counterparty import
 * 3. Report Setup        - optionally select initial report definitions to create
 *
 * Execute phase (single page, all backend work):
 * 4. Execute             - publishes counterparties, publishes organisation bundle,
 *                          creates selected reports, marks party active
 *
 * 5. Summary             - shows results; no further backend calls
 *
 * This wizard appears automatically on first login when the selected party's
 * status is 'Inactive'. It sets the party status to 'Active' on completion.
 */
class PartyProvisioningWizard final : public QWizard {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.party_provisioning_wizard";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum PageId {
        Page_Welcome,
        Page_CounterpartySetup,
        Page_ReportSetup,
        Page_Execute,
        Page_Summary
    };

    /**
     * @brief Specification for an initial report definition to create during provisioning.
     */
    struct ReportSpec {
        std::string name;
        std::string description;
        std::string schedule_expression;
        std::string report_type;
        std::string concurrency_policy;
    };

    explicit PartyProvisioningWizard(ClientManager* clientManager, QWidget* parent = nullptr);

    ~PartyProvisioningWizard() override = default;

    ClientManager* clientManager() const {
        return clientManager_;
    }

    QString selectedBundleCode() const {
        return selectedBundleCode_;
    }
    void setSelectedBundleCode(const QString& code) {
        selectedBundleCode_ = code;
    }

    QString leiDatasetSize() const {
        return leiDatasetSize_;
    }
    void setLeiDatasetSize(const QString& size) {
        leiDatasetSize_ = size;
    }

    bool organisationPublished() const {
        return organisationPublished_;
    }
    void setOrganisationPublished(bool v) {
        organisationPublished_ = v;
    }

    std::vector<ReportSpec> selectedReports() const {
        return selectedReports_;
    }
    void setSelectedReports(std::vector<ReportSpec> r) {
        selectedReports_ = std::move(r);
    }

signals:
    void provisioningCompleted();

private:
    void setupPages();

    ClientManager* clientManager_;
    QString selectedBundleCode_ = "base";
    QString leiDatasetSize_ = "small";
    bool organisationPublished_ = false;
    std::vector<ReportSpec> selectedReports_;
};

// Forward declarations
class PartyWelcomePage;
class PartyCounterpartySetupPage;
class PartyReportSetupPage;
class PartyExecutePage;
class PartyApplyAndSummaryPage;

/**
 * @brief Welcome page for the party provisioning wizard.
 */
class PartyWelcomePage final : public QWizardPage {
    Q_OBJECT

public:
    explicit PartyWelcomePage(PartyProvisioningWizard* wizard);

private:
    void setupUI();
    PartyProvisioningWizard* wizard_;
};

/**
 * @brief Page for selecting the GLEIF counterparty dataset size.
 *
 * Counterparties are imported from the full GLEIF dataset (no root LEI filtering).
 * The user selects small or large depending on their data needs.
 */
class PartyCounterpartySetupPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit PartyCounterpartySetupPage(PartyProvisioningWizard* wizard);
    bool validatePage() override;

private:
    void setupUI();
    PartyProvisioningWizard* wizard_;
    QComboBox* datasetSizeCombo_;
};

/**
 * @brief Page for selecting which initial report definitions to create.
 *
 * Loads available templates from the reporting service on entry via the
 * reporting.v1.report-definition-templates.list NATS endpoint.
 */
class PartyReportSetupPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.party_report_setup_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PartyReportSetupPage(PartyProvisioningWizard* wizard);
    void initializePage() override;
    bool validatePage() override;

private:
    void setupUI();
    void loadTemplates();
    void
    populateList(const std::vector<ores::dq::messaging::dq_report_definition_template>& templates);

    PartyProvisioningWizard* wizard_;
    QLabel* loadingLabel_;
    QLabel* errorLabel_;
    QListWidget* reportList_;
};

/**
 * @brief Executes all backend work in sequence and shows live progress.
 *
 * Phase 1: Publish counterparties from the GLEIF dataset (opted-in dataset only,
 *          using the opted_in_datasets filter to target gleif.lei_counterparties.{size}).
 *          Workflow progress shown via WorkflowStepsWidget.
 * Phase 2: Publish organisation bundle (business units, portfolios, trading books)
 *          with party_id param scoped to the current party.
 *          Workflow progress shown via WorkflowStepsWidget.
 * Phase 3: Create selected report definitions (sequential, non-workflow).
 * Phase 4: Mark party status as Active.
 *
 * The Next button is only enabled after all phases complete.
 */
class PartyExecutePage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.party_execute_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PartyExecutePage(PartyProvisioningWizard* wizard);
    void initializePage() override;
    bool isComplete() const override;

private slots:
    void onCounterpartyWorkflowComplete(bool success);
    void onOrgWorkflowComplete(bool success);
    void onFxSpotConfigsWorkflowComplete(bool success);
    void onFxDriverRatesWorkflowComplete(bool success);

private:
    void startCounterpartyPublish();
    void startOrgPublish();
    void startReportInstall();
    void startFxSpotConfigsPublish();
    void startFxDriverRatesPublish();
    void startActivate();
    void markFailed(const QString& errorMsg);
    void appendLog(const QString& msg);

    PartyProvisioningWizard* wizard_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    WorkflowStepsWidget* stepsWidget_ = nullptr;
    QTextEdit* logOutput_;

    bool allComplete_ = false;
    bool allSuccess_ = false;
    std::string publishedBy_;
};

/**
 * @brief Final summary page; purely informational, no backend calls.
 */
class PartyApplyAndSummaryPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.party_apply_and_summary_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PartyApplyAndSummaryPage(PartyProvisioningWizard* wizard);
    void initializePage() override;

private:
    void setupUI();

    PartyProvisioningWizard* wizard_;
    QLabel* summaryLabel_;
};

}

#endif
