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
#include <QLineEdit>
#include <QProgressBar>
#include <QTextEdit>
#include <QPushButton>
#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

class ClientDatasetBundleModel;

/**
 * @brief Wizard for first-time tenant setup after provisioning.
 *
 * Guides a tenant admin through initial setup:
 * 1. Welcome          - explains the setup process
 * 2. Bundle Selection - choose a reference data bundle to publish
 * 3. Bundle Install   - publish the selected bundle
 * 4. Account Setup    - create the first party admin account
 * 5. Summary          - clear bootstrap flag and show results
 *
 * This wizard appears automatically on first login to a tenant that is
 * in bootstrap mode. It clears the bootstrap flag on completion or cancel.
 * Party-level setup (data sources, parties, reports) is deferred to the
 * PartyProvisioningWizard, which runs after the party admin first logs in.
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
        Page_AccountSetup,
        Page_Summary
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

    QString newAccountUsername() const { return newAccountUsername_; }
    void setNewAccountUsername(const QString& u) { newAccountUsername_ = u; }

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
    QString newAccountUsername_;
};

// Forward declarations
class ProvisioningWelcomePage;
class BundleSelectionPage;
class BundleInstallPage;
class AccountSetupPage;
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
 * @brief Page for creating the first party admin account.
 *
 * The account is created without a party association. The party_setup_mode
 * flag guides this user into the PartyProvisioningWizard on their first login.
 */
class AccountSetupPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.account_setup_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit AccountSetupPage(TenantProvisioningWizard* wizard);
    bool validatePage() override;
    bool isComplete() const override;

private:
    void setupUI();

    TenantProvisioningWizard* wizard_;
    QLineEdit* usernameEdit_;
    QLineEdit* passwordEdit_;
    QLineEdit* confirmPasswordEdit_;
    QLineEdit* emailEdit_;
    QLabel* statusLabel_;
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
