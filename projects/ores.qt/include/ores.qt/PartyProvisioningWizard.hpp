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
#include <QLineEdit>
#include <QProgressBar>
#include <QRadioButton>
#include <QSpinBox>
#include <QCheckBox>
#include <QTextEdit>
#include <QListWidget>
#include <optional>
#include <cstdint>
#include <vector>
#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

class LeiEntityPicker;

/**
 * @brief Wizard for first-time party setup after a tenant's first party admin logs in.
 *
 * Guides the party admin through setting up the operational party structure:
 * 1. Welcome             - explains the setup process
 * 2. Data Source         - choose between GLEIF registry and synthetic data
 * 3. Party Setup         - (GLEIF only) select a root LEI entity
 * 4. Counterparty Setup  - (GLEIF only) informational placeholder
 * 5. Organisation Setup  - populate parties, counterparties, and org structure
 * 6. Report Setup        - select initial report definitions to create
 * 7. Report Install      - create the selected report definitions
 * 8. Summary             - set party status to 'Active' and show results
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
        Page_DataSourceSelection,
        Page_PartySetup,
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

    enum class DataSourceMode {
        gleif,
        synthetic
    };

    explicit PartyProvisioningWizard(
        ClientManager* clientManager,
        QWidget* parent = nullptr);

    ~PartyProvisioningWizard() override = default;

    ClientManager* clientManager() const { return clientManager_; }

    QString selectedBundleCode() const { return selectedBundleCode_; }
    void setSelectedBundleCode(const QString& code) { selectedBundleCode_ = code; }

    /**
     * @brief When true, the party hierarchy already exists from tenant provisioning.
     * The wizard skips data source selection and party setup, going straight to
     * counterparty setup.
     */
    bool partiesAlreadyProvisioned() const { return partiesAlreadyProvisioned_; }
    void setPartiesAlreadyProvisioned(bool v) { partiesAlreadyProvisioned_ = v; }

    QString rootLei() const { return rootLei_; }
    void setRootLei(const QString& lei) { rootLei_ = lei; }

    QString rootLeiName() const { return rootLeiName_; }
    void setRootLeiName(const QString& name) { rootLeiName_ = name; }

    QString leiDatasetSize() const { return leiDatasetSize_; }
    void setLeiDatasetSize(const QString& size) { leiDatasetSize_ = size; }

    bool organisationPublished() const { return organisationPublished_; }
    void setOrganisationPublished(bool v) { organisationPublished_ = v; }

    DataSourceMode dataSourceMode() const { return dataSourceMode_; }
    void setDataSourceMode(DataSourceMode m) { dataSourceMode_ = m; }

    // --- Synthetic generation options (set by DataSourceSelectionPage) ---

    QString syntheticCountry() const { return syntheticCountry_; }
    void setSyntheticCountry(const QString& c) { syntheticCountry_ = c; }

    int syntheticPartyCount() const { return syntheticPartyCount_; }
    void setSyntheticPartyCount(int c) { syntheticPartyCount_ = c; }

    int syntheticPartyMaxDepth() const { return syntheticPartyMaxDepth_; }
    void setSyntheticPartyMaxDepth(int d) { syntheticPartyMaxDepth_ = d; }

    int syntheticCounterpartyCount() const { return syntheticCounterpartyCount_; }
    void setSyntheticCounterpartyCount(int c) { syntheticCounterpartyCount_ = c; }

    int syntheticCounterpartyMaxDepth() const { return syntheticCounterpartyMaxDepth_; }
    void setSyntheticCounterpartyMaxDepth(int d) { syntheticCounterpartyMaxDepth_ = d; }

    int syntheticPortfolioLeafCount() const { return syntheticPortfolioLeafCount_; }
    void setSyntheticPortfolioLeafCount(int c) { syntheticPortfolioLeafCount_ = c; }

    int syntheticPortfolioMaxDepth() const { return syntheticPortfolioMaxDepth_; }
    void setSyntheticPortfolioMaxDepth(int d) { syntheticPortfolioMaxDepth_ = d; }

    int syntheticBooksPerPortfolio() const { return syntheticBooksPerPortfolio_; }
    void setSyntheticBooksPerPortfolio(int c) { syntheticBooksPerPortfolio_ = c; }

    int syntheticBusinessUnitCount() const { return syntheticBusinessUnitCount_; }
    void setSyntheticBusinessUnitCount(int c) { syntheticBusinessUnitCount_ = c; }

    int syntheticBusinessUnitMaxDepth() const { return syntheticBusinessUnitMaxDepth_; }
    void setSyntheticBusinessUnitMaxDepth(int d) { syntheticBusinessUnitMaxDepth_ = d; }

    bool syntheticGenerateAddresses() const { return syntheticGenerateAddresses_; }
    void setSyntheticGenerateAddresses(bool v) { syntheticGenerateAddresses_ = v; }

    int syntheticContactsPerParty() const { return syntheticContactsPerParty_; }
    void setSyntheticContactsPerParty(int c) { syntheticContactsPerParty_ = c; }

    int syntheticContactsPerCounterparty() const { return syntheticContactsPerCounterparty_; }
    void setSyntheticContactsPerCounterparty(int c) { syntheticContactsPerCounterparty_ = c; }

    bool syntheticGenerateIdentifiers() const { return syntheticGenerateIdentifiers_; }
    void setSyntheticGenerateIdentifiers(bool v) { syntheticGenerateIdentifiers_ = v; }

    std::optional<std::uint64_t> syntheticSeed() const { return syntheticSeed_; }
    void setSyntheticSeed(std::optional<std::uint64_t> s) { syntheticSeed_ = s; }

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
    bool partiesAlreadyProvisioned_ = false;
    QString rootLei_;
    QString rootLeiName_;
    QString leiDatasetSize_ = "large";
    bool organisationPublished_ = false;
    DataSourceMode dataSourceMode_ = DataSourceMode::gleif;

    // Synthetic generation options
    QString syntheticCountry_ = "GB";
    int syntheticPartyCount_ = 5;
    int syntheticPartyMaxDepth_ = 3;
    int syntheticCounterpartyCount_ = 10;
    int syntheticCounterpartyMaxDepth_ = 3;
    int syntheticPortfolioLeafCount_ = 8;
    int syntheticPortfolioMaxDepth_ = 4;
    int syntheticBooksPerPortfolio_ = 2;
    int syntheticBusinessUnitCount_ = 10;
    int syntheticBusinessUnitMaxDepth_ = 2;
    bool syntheticGenerateAddresses_ = true;
    int syntheticContactsPerParty_ = 2;
    int syntheticContactsPerCounterparty_ = 1;
    bool syntheticGenerateIdentifiers_ = true;
    std::optional<std::uint64_t> syntheticSeed_;

    std::vector<ReportSpec> selectedReports_;
};

// Forward declarations
class PartyWelcomePage;
class PartyDataSourceSelectionPage;
class PartySetupPage;
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
    int nextId() const override;
    void initializePage() override;

private:
    void setupUI();
    QLabel* stepsLabel_ = nullptr;
    PartyProvisioningWizard* wizard_;
};

/**
 * @brief Page for choosing between GLEIF registry and synthetic data generation.
 */
class PartyDataSourceSelectionPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit PartyDataSourceSelectionPage(PartyProvisioningWizard* wizard);
    bool validatePage() override;
    int nextId() const override;

private:
    void setupUI();
    void onModeChanged();

    PartyProvisioningWizard* wizard_;
    QRadioButton* gleifRadio_;
    QRadioButton* syntheticRadio_;
    QWidget* syntheticOptions_;

    // Basic counts
    QComboBox* countryCombo_;
    QSpinBox* partyCountSpin_;
    QSpinBox* counterpartyCountSpin_;
    QSpinBox* portfolioLeafCountSpin_;
    QSpinBox* booksPerPortfolioSpin_;
    QSpinBox* businessUnitCountSpin_;

    // Hierarchy depths
    QSpinBox* partyMaxDepthSpin_;
    QSpinBox* counterpartyMaxDepthSpin_;
    QSpinBox* portfolioMaxDepthSpin_;
    QSpinBox* businessUnitMaxDepthSpin_;

    // Contact and identifier options
    QSpinBox* contactsPerPartySpin_;
    QSpinBox* contactsPerCounterpartySpin_;
    QCheckBox* generateAddressesCheck_;
    QCheckBox* generateIdentifiersCheck_;

    // Reproducibility
    QLineEdit* seedEdit_;
};

/**
 * @brief Page for optional root party selection via LEI entity picker.
 */
class PartySetupPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit PartySetupPage(PartyProvisioningWizard* wizard);
    void initializePage() override;
    bool validatePage() override;

private:
    void setupUI();

    PartyProvisioningWizard* wizard_;
    LeiEntityPicker* leiPicker_;
    QComboBox* datasetSizeCombo_;
    QLabel* instructionLabel_;
    bool leiLoaded_ = false;
};

/**
 * @brief Informational page about counterparty import (placeholder).
 */
class PartyCounterpartySetupPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit PartyCounterpartySetupPage(PartyProvisioningWizard* wizard);

private:
    void setupUI();
    PartyProvisioningWizard* wizard_;
};

/**
 * @brief Page for async publication of the organisation dataset bundle.
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
    void startBundlePublish();
    void startSyntheticGeneration();
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
 */
class PartyReportSetupPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit PartyReportSetupPage(PartyProvisioningWizard* wizard);
    bool validatePage() override;
    int nextId() const override;

private:
    void setupUI();

    PartyProvisioningWizard* wizard_;
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
