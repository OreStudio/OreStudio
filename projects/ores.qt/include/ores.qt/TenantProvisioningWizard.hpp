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
#include <QComboBox>
#include <QLabel>
#include <QProgressBar>
#include <QRadioButton>
#include <QSpinBox>
#include <QCheckBox>
#include <QTextEdit>
#include <QPushButton>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

class ClientDatasetBundleModel;
class LeiEntityPicker;

/**
 * @brief Wizard for first-time tenant setup after provisioning.
 *
 * Guides a tenant admin through initial setup:
 * 1. Welcome - explains the setup process
 * 2. Bundle Selection - choose a reference data bundle to publish
 * 3. Bundle Install - publish the selected bundle
 * 4. Party Setup - optionally select a root LEI entity
 * 5. Counterparty Setup - informational placeholder
 * 6. Apply & Summary - clear bootstrap flag and show summary
 *
 * This wizard appears automatically on first login to a tenant that is
 * in bootstrap mode. It clears the bootstrap flag on completion or cancel.
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
        Page_CounterpartySetup,
        Page_OrganisationSetup,
        Page_Summary
    };

    enum class DataSourceMode {
        gleif,
        synthetic
    };

    explicit TenantProvisioningWizard(
        ClientManager* clientManager,
        QWidget* parent = nullptr);

    ~TenantProvisioningWizard() override = default;

    ClientManager* clientManager() const { return clientManager_; }

    QString selectedBundleCode() const { return selectedBundleCode_; }
    void setSelectedBundleCode(const QString& code) { selectedBundleCode_ = code; }

    QString selectedBundleName() const { return selectedBundleName_; }
    void setSelectedBundleName(const QString& name) { selectedBundleName_ = name; }

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

    // Synthetic generation options (set by DataSourceSelectionPage)
    QString syntheticCountry() const { return syntheticCountry_; }
    void setSyntheticCountry(const QString& c) { syntheticCountry_ = c; }
    int syntheticPartyCount() const { return syntheticPartyCount_; }
    void setSyntheticPartyCount(int c) { syntheticPartyCount_ = c; }
    int syntheticCounterpartyCount() const { return syntheticCounterpartyCount_; }
    void setSyntheticCounterpartyCount(int c) { syntheticCounterpartyCount_ = c; }
    int syntheticPortfolioLeafCount() const { return syntheticPortfolioLeafCount_; }
    void setSyntheticPortfolioLeafCount(int c) { syntheticPortfolioLeafCount_ = c; }
    int syntheticBooksPerPortfolio() const { return syntheticBooksPerPortfolio_; }
    void setSyntheticBooksPerPortfolio(int c) { syntheticBooksPerPortfolio_ = c; }
    int syntheticBusinessUnitCount() const { return syntheticBusinessUnitCount_; }
    void setSyntheticBusinessUnitCount(int c) { syntheticBusinessUnitCount_ = c; }
    bool syntheticGenerateAddresses() const { return syntheticGenerateAddresses_; }
    void setSyntheticGenerateAddresses(bool v) { syntheticGenerateAddresses_ = v; }
    bool syntheticGenerateIdentifiers() const { return syntheticGenerateIdentifiers_; }
    void setSyntheticGenerateIdentifiers(bool v) { syntheticGenerateIdentifiers_ = v; }

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
    QString rootLei_;
    QString rootLeiName_;
    QString leiDatasetSize_ = "large";
    bool organisationPublished_ = false;
    DataSourceMode dataSourceMode_ = DataSourceMode::gleif;
    QString syntheticCountry_ = "GB";
    int syntheticPartyCount_ = 5;
    int syntheticCounterpartyCount_ = 10;
    int syntheticPortfolioLeafCount_ = 8;
    int syntheticBooksPerPortfolio_ = 2;
    int syntheticBusinessUnitCount_ = 10;
    bool syntheticGenerateAddresses_ = true;
    bool syntheticGenerateIdentifiers_ = true;
};

// Forward declarations
class ProvisioningWelcomePage;
class BundleSelectionPage;
class BundleInstallPage;
class DataSourceSelectionPage;
class PartySetupPage;
class CounterpartySetupPage;
class OrganisationSetupPage;
class ApplyAndSummaryPage;

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
 * @brief Page for choosing between GLEIF registry and synthetic data generation.
 */
class DataSourceSelectionPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit DataSourceSelectionPage(TenantProvisioningWizard* wizard);
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
    QSpinBox* counterpartyCountSpin_;
    QSpinBox* portfolioLeafCountSpin_;
    QSpinBox* booksPerPortfolioSpin_;
    QSpinBox* businessUnitCountSpin_;
    QCheckBox* generateAddressesCheck_;
    QCheckBox* generateIdentifiersCheck_;
};

/**
 * @brief Page for optional root party selection via LEI entity picker.
 */
class PartySetupPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit PartySetupPage(TenantProvisioningWizard* wizard);
    void initializePage() override;
    bool validatePage() override;

private:
    void setupUI();

    TenantProvisioningWizard* wizard_;
    LeiEntityPicker* leiPicker_;
    QComboBox* datasetSizeCombo_;
    QLabel* instructionLabel_;
    bool leiLoaded_ = false;
};

/**
 * @brief Informational page about counterparty import (placeholder).
 */
class CounterpartySetupPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit CounterpartySetupPage(TenantProvisioningWizard* wizard);

private:
    void setupUI();
    TenantProvisioningWizard* wizard_;
};

/**
 * @brief Page for async publication of the organisation dataset bundle.
 *
 * Publishes business units, portfolios, and books for the target tenant.
 * Requires a root party to exist (set up in PartySetupPage).
 */
class OrganisationSetupPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.organisation_setup_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OrganisationSetupPage(TenantProvisioningWizard* wizard);
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
class ApplyAndSummaryPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.apply_and_summary_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ApplyAndSummaryPage(TenantProvisioningWizard* wizard);
    void initializePage() override;

private:
    void setupUI();

    TenantProvisioningWizard* wizard_;
    QLabel* summaryLabel_;
};

}

#endif
