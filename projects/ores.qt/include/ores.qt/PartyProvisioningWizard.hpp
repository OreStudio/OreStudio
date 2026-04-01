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

#include <QWizard>
#include <QWizardPage>
#include <QComboBox>
#include <QLabel>
#include <QProgressBar>
#include <QTextEdit>
#include <QListWidget>
#include <optional>
#include <cstdint>
#include <vector>
#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.reporting.api/domain/report_definition_template.hpp"

namespace ores::qt {

/**
 * @brief Wizard for setting up a party after it has been created by tenant provisioning.
 *
 * Assumes the party hierarchy already exists. Guides the party admin through:
 * 1. Welcome             - explains the setup process
 * 2. Counterparty Setup  - select dataset size and import GLEIF counterparties
 * 3. Organisation Setup  - publish business units, portfolios, and trading books
 * 4. Report Setup        - select initial report definitions to create
 * 5. Report Install      - create the selected report definitions
 * 6. Summary             - set party status to 'Active' and show results
 *
 * This wizard appears automatically on first login when the selected party's
 * status is 'Inactive'. It sets the party status to 'Active' on completion.
 */
class PartyProvisioningWizard final : public QWizard {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.party_provisioning_wizard";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum PageId {
        Page_Welcome,
        Page_CounterpartySetup,
        Page_OrganisationSetup,
        Page_ReportSetup,
        Page_ReportInstall,
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

    explicit PartyProvisioningWizard(
        ClientManager* clientManager,
        QWidget* parent = nullptr);

    ~PartyProvisioningWizard() override = default;

    ClientManager* clientManager() const { return clientManager_; }

    QString selectedBundleCode() const { return selectedBundleCode_; }
    void setSelectedBundleCode(const QString& code) { selectedBundleCode_ = code; }

    QString leiDatasetSize() const { return leiDatasetSize_; }
    void setLeiDatasetSize(const QString& size) { leiDatasetSize_ = size; }

    bool organisationPublished() const { return organisationPublished_; }
    void setOrganisationPublished(bool v) { organisationPublished_ = v; }

    std::vector<ReportSpec> selectedReports() const { return selectedReports_; }
    void setSelectedReports(std::vector<ReportSpec> r) { selectedReports_ = std::move(r); }

    /**
     * @brief Sets the current party's status to 'Active' via save_party_request.
     *
     * Called from the summary page on successful completion of the wizard.
     * Returns false (and logs a warning) if the party cannot be activated.
     */
    [[nodiscard]] bool markPartyActive();

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
class PartyOrganisationSetupPage;
class PartyReportSetupPage;
class PartyReportInstallPage;
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
 * @brief Page for async publication of counterparties and organisation structure.
 */
class PartyOrganisationSetupPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.party_organisation_setup_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PartyOrganisationSetupPage(PartyProvisioningWizard* wizard);
    void initializePage() override;
    bool isComplete() const override;

private:
    void startPublish();
    void appendLog(const QString& message);

    PartyProvisioningWizard* wizard_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    QTextEdit* logOutput_;
    bool publishComplete_ = false;
    bool publishSuccess_ = false;
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
    inline static std::string_view logger_name =
        "ores.qt.party_report_setup_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PartyReportSetupPage(PartyProvisioningWizard* wizard);
    void initializePage() override;
    bool validatePage() override;
    int nextId() const override;

private:
    void setupUI();
    void loadTemplates();
    void populateList(const std::vector<ores::reporting::domain::report_definition_template>& templates);

    PartyProvisioningWizard* wizard_;
    QLabel* loadingLabel_;
    QLabel* errorLabel_;
    QListWidget* reportList_;
};

/**
 * @brief Page that asynchronously creates the selected report definitions.
 */
class PartyReportInstallPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.party_report_install_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PartyReportInstallPage(PartyProvisioningWizard* wizard);
    void initializePage() override;
    bool isComplete() const override;

private:
    void startInstall();
    void appendLog(const QString& message);

    PartyProvisioningWizard* wizard_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    QTextEdit* logOutput_;
    bool installComplete_ = false;
    bool installSuccess_ = false;
};

/**
 * @brief Final summary page that clears the party setup flag.
 */
class PartyApplyAndSummaryPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.party_apply_and_summary_page";

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
