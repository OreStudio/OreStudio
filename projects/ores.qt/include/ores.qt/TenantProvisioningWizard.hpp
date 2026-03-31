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
#ifndef ORES_QT_TENANT_PROVISIONING_WIZARD_HPP
#define ORES_QT_TENANT_PROVISIONING_WIZARD_HPP

#include <QWizard>
#include <QWizardPage>
#include <QCheckBox>
#include <QComboBox>
#include <QLabel>
#include <QLineEdit>
#include <QProgressBar>
#include <QRadioButton>
#include <QSpinBox>
#include <QTextEdit>
#include <QPushButton>
#include <cstdint>
#include <optional>
#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

class ClientDatasetBundleModel;
class LeiEntityPicker;

/**
 * @brief Wizard for first-time tenant setup after provisioning.
 *
 * Guides a tenant admin through initial setup:
 * 1. Welcome              - explains the setup process
 * 2. Bundle Selection     - choose a reference data bundle to publish
 * 3. Bundle Install       - publish the selected bundle
 * 4. Data Source          - choose GLEIF registry or synthetic data
 * 5. Party Setup          - (GLEIF only) select root LEI entity
 * 6. Party Organisation   - publish party hierarchy and org structure;
 *                           associates the tenant admin with all created parties
 * 7. Summary              - clear bootstrap flag and show results
 *
 * This wizard appears automatically on first login to a tenant that is in
 * bootstrap mode. It clears the bootstrap flag on completion or cancel.
 * Per-party operational setup (data, reports) is handled by
 * PartyProvisioningWizard, which fires automatically when the tenant admin
 * logs in to each Inactive party.
 */
class TenantProvisioningWizard final : public QWizard {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.tenant_provisioning_wizard";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum PageId {
        Page_Welcome,
        Page_BundleSelection,
        Page_BundleInstall,
        Page_DataSourceSelection,
        Page_PartySetup,
        Page_PartyOrganisation,
        Page_Summary
    };

    enum class DataSourceMode { gleif, synthetic };

    explicit TenantProvisioningWizard(
        ClientManager* clientManager,
        QWidget* parent = nullptr);

    ~TenantProvisioningWizard() override = default;

    ClientManager* clientManager() const { return clientManager_; }

    QString selectedBundleCode() const { return selectedBundleCode_; }
    void setSelectedBundleCode(const QString& code) { selectedBundleCode_ = code; }

    QString selectedBundleName() const { return selectedBundleName_; }
    void setSelectedBundleName(const QString& name) { selectedBundleName_ = name; }

    // --- GLEIF / synthetic party setup state ---
    DataSourceMode dataSourceMode() const { return dataSourceMode_; }
    void setDataSourceMode(DataSourceMode m) { dataSourceMode_ = m; }

    QString rootLei() const { return rootLei_; }
    void setRootLei(const QString& lei) { rootLei_ = lei; }

    QString rootLeiName() const { return rootLeiName_; }
    void setRootLeiName(const QString& name) { rootLeiName_ = name; }

    QString leiDatasetSize() const { return leiDatasetSize_; }
    void setLeiDatasetSize(const QString& size) { leiDatasetSize_ = size; }

    QString syntheticCountry() const { return syntheticCountry_; }
    void setSyntheticCountry(const QString& c) { syntheticCountry_ = c; }

    int syntheticPartyCount() const { return syntheticPartyCount_; }
    void setSyntheticPartyCount(int v) { syntheticPartyCount_ = v; }

    int syntheticPartyMaxDepth() const { return syntheticPartyMaxDepth_; }
    void setSyntheticPartyMaxDepth(int v) { syntheticPartyMaxDepth_ = v; }

    int syntheticCounterpartyCount() const { return syntheticCounterpartyCount_; }
    void setSyntheticCounterpartyCount(int v) { syntheticCounterpartyCount_ = v; }

    int syntheticCounterpartyMaxDepth() const { return syntheticCounterpartyMaxDepth_; }
    void setSyntheticCounterpartyMaxDepth(int v) { syntheticCounterpartyMaxDepth_ = v; }

    int syntheticPortfolioLeafCount() const { return syntheticPortfolioLeafCount_; }
    void setSyntheticPortfolioLeafCount(int v) { syntheticPortfolioLeafCount_ = v; }

    int syntheticPortfolioMaxDepth() const { return syntheticPortfolioMaxDepth_; }
    void setSyntheticPortfolioMaxDepth(int v) { syntheticPortfolioMaxDepth_ = v; }

    int syntheticBooksPerPortfolio() const { return syntheticBooksPerPortfolio_; }
    void setSyntheticBooksPerPortfolio(int v) { syntheticBooksPerPortfolio_ = v; }

    int syntheticBusinessUnitCount() const { return syntheticBusinessUnitCount_; }
    void setSyntheticBusinessUnitCount(int v) { syntheticBusinessUnitCount_ = v; }

    int syntheticBusinessUnitMaxDepth() const { return syntheticBusinessUnitMaxDepth_; }
    void setSyntheticBusinessUnitMaxDepth(int v) { syntheticBusinessUnitMaxDepth_ = v; }

    bool syntheticGenerateAddresses() const { return syntheticGenerateAddresses_; }
    void setSyntheticGenerateAddresses(bool v) { syntheticGenerateAddresses_ = v; }

    int syntheticContactsPerParty() const { return syntheticContactsPerParty_; }
    void setSyntheticContactsPerParty(int v) { syntheticContactsPerParty_ = v; }

    int syntheticContactsPerCounterparty() const { return syntheticContactsPerCounterparty_; }
    void setSyntheticContactsPerCounterparty(int v) { syntheticContactsPerCounterparty_ = v; }

    bool syntheticGenerateIdentifiers() const { return syntheticGenerateIdentifiers_; }
    void setSyntheticGenerateIdentifiers(bool v) { syntheticGenerateIdentifiers_ = v; }

    std::optional<std::uint64_t> syntheticSeed() const { return syntheticSeed_; }
    void setSyntheticSeed(std::optional<std::uint64_t> v) { syntheticSeed_ = v; }

    int partiesLinkedCount() const { return partiesLinkedCount_; }
    void setPartiesLinkedCount(int n) { partiesLinkedCount_ = n; }

    /**
     * @brief Clears the system.bootstrap_mode flag for the current tenant.
     */
    void clearBootstrapFlag();

signals:
    void provisioningCompleted();

private:
    void setupPages();

    ClientManager* clientManager_;
    QString selectedBundleCode_;
    QString selectedBundleName_;

    // Party setup state
    DataSourceMode dataSourceMode_ = DataSourceMode::gleif;
    QString rootLei_;
    QString rootLeiName_;
    QString leiDatasetSize_;
    QString syntheticCountry_;
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
    int partiesLinkedCount_ = 0;
};

// Forward declarations
class ProvisioningWelcomePage;
class BundleSelectionPage;
class BundleInstallPage;
class TenantDataSourceSelectionPage;
class TenantPartySetupPage;
class TenantPartyOrganisationPage;
class TenantApplyAndSummaryPage;

/**
 * @brief Welcome page explaining what the provisioning wizard does.
 */
class ProvisioningWelcomePage final : public QWizardPage {
    Q_OBJECT

public:
    explicit ProvisioningWelcomePage(TenantProvisioningWizard* wizard);

private:
    void setupUI();
    TenantProvisioningWizard* wizard_;
};

/**
 * @brief Page for selecting a dataset bundle to publish.
 */
class BundleSelectionPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit BundleSelectionPage(TenantProvisioningWizard* wizard);
    void initializePage() override;
    bool validatePage() override;
    bool isComplete() const override;

private:
    void setupUI();
    void onBundleChanged(int index);

    TenantProvisioningWizard* wizard_;
    ClientDatasetBundleModel* bundleModel_;
    QComboBox* bundleCombo_;
    QLabel* descriptionLabel_;
    QLabel* statusLabel_;
};

/**
 * @brief Page for async publication of the selected bundle.
 */
class BundleInstallPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.bundle_install_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit BundleInstallPage(TenantProvisioningWizard* wizard);
    void initializePage() override;
    bool isComplete() const override;

private:
    void startPublish();
    void appendLog(const QString& message);

    TenantProvisioningWizard* wizard_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    QTextEdit* logOutput_;
    bool publishComplete_ = false;
    bool publishSuccess_ = false;
};

/**
 * @brief Page for choosing GLEIF registry or synthetic data for party setup.
 */
class TenantDataSourceSelectionPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit TenantDataSourceSelectionPage(TenantProvisioningWizard* wizard);
    bool validatePage() override;
    int nextId() const override;

private:
    void setupUI();
    void onModeChanged();

    TenantProvisioningWizard* wizard_;
    QRadioButton* gleifRadio_;
    QRadioButton* syntheticRadio_;
    QWidget* syntheticOptions_;
    QComboBox* countryCombo_;
    QSpinBox* partyCountSpin_;
    QSpinBox* partyMaxDepthSpin_;
    QSpinBox* counterpartyCountSpin_;
    QSpinBox* counterpartyMaxDepthSpin_;
    QSpinBox* portfolioLeafCountSpin_;
    QSpinBox* portfolioMaxDepthSpin_;
    QSpinBox* booksPerPortfolioSpin_;
    QSpinBox* businessUnitCountSpin_;
    QSpinBox* businessUnitMaxDepthSpin_;
    QSpinBox* contactsPerPartySpin_;
    QSpinBox* contactsPerCounterpartySpin_;
    QCheckBox* generateAddressesCheck_;
    QCheckBox* generateIdentifiersCheck_;
    QLineEdit* seedEdit_;
};

/**
 * @brief Page for selecting a root LEI entity (GLEIF mode only).
 */
class TenantPartySetupPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit TenantPartySetupPage(TenantProvisioningWizard* wizard);
    void initializePage() override;
    bool validatePage() override;

private:
    void setupUI();

    TenantProvisioningWizard* wizard_;
    QLabel* instructionLabel_;
    QComboBox* datasetSizeCombo_;
    LeiEntityPicker* leiPicker_;
    bool leiLoaded_ = false;
};

/**
 * @brief Page that publishes party hierarchy (GLEIF or synthetic) and
 *        associates the tenant admin with all created parties.
 */
class TenantPartyOrganisationPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.tenant_party_organisation_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit TenantPartyOrganisationPage(TenantProvisioningWizard* wizard);
    void initializePage() override;
    bool isComplete() const override;

private:
    void startPublish();
    void startBundlePublish();
    void startSyntheticGeneration();
    void appendLog(const QString& message);

    TenantProvisioningWizard* wizard_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    QTextEdit* logOutput_;
    bool publishComplete_ = false;
    bool publishSuccess_ = false;
};

/**
 * @brief Final summary page that clears the bootstrap flag.
 */
class TenantApplyAndSummaryPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.tenant_apply_and_summary_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit TenantApplyAndSummaryPage(TenantProvisioningWizard* wizard);
    void initializePage() override;

private:
    void setupUI();

    TenantProvisioningWizard* wizard_;
    QLabel* summaryLabel_;
};

}

#endif
