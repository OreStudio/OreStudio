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
#ifndef ORES_QT_SYSTEM_PROVISIONER_WIZARD_HPP
#define ORES_QT_SYSTEM_PROVISIONER_WIZARD_HPP

#include <vector>
#include <QWizard>
#include <QWizardPage>
#include <QLineEdit>
#include <QLabel>
#include <QRadioButton>
#include <QButtonGroup>
#include <QProgressBar>
#include <QTextEdit>
#include <QCheckBox>
#include <QComboBox>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {


/**
 * @brief Result of a provisioning operation.
 */
struct ProvisioningResult {
    bool success = false;
    QString error_message;
    boost::uuids::uuid admin_account_id;
    QStringList log_messages;
};

/**
 * @brief Wizard for initial system provisioning when in bootstrap mode.
 *
 * Multi-page wizard that guides users through:
 * 1. Creating the initial administrator account
 * 2. Selecting a dataset bundle to provision
 * 3. Applying the provisioning and showing progress
 *
 * This wizard should only be shown when the system is in bootstrap mode
 * (i.e., no administrator account exists yet).
 */
class SystemProvisionerWizard final : public QWizard {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.system_provisioner_wizard";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
signals:
    /**
     * @brief Emitted when provisioning is successfully completed.
     *
     * Connect to this signal to proceed with normal login flow.
     *
     * @param username The username of the created admin account
     */
    void provisioningCompleted(const QString& username);

    /**
     * @brief Emitted when provisioning fails.
     *
     * @param errorMessage Description of the failure
     */
    void provisioningFailed(const QString& errorMessage);

public:
    // Page IDs
    enum PageId {
        Page_Welcome,
        Page_AdminAccount,
        Page_BundleSelection,
        Page_Apply
    };

    explicit SystemProvisionerWizard(
        ClientManager* clientManager,
        const std::vector<BootstrapBundleInfo>& bundles,
        QWidget* parent = nullptr);

    ~SystemProvisionerWizard() override = default;

    // Accessors for wizard pages
    ClientManager* clientManager() const { return clientManager_; }

    // Admin account data
    QString adminUsername() const { return adminUsername_; }
    QString adminEmail() const { return adminEmail_; }
    QString adminPassword() const { return adminPassword_; }
    void setAdminCredentials(const QString& username, const QString& email,
                             const QString& password);

    // Bundle selection
    const std::vector<BootstrapBundleInfo>& bundles() const { return bundles_; }
    QString selectedBundleCode() const { return selectedBundleCode_; }
    void setSelectedBundleCode(const QString& code) { selectedBundleCode_ = code; }

    // Created admin account ID (set after successful creation)
    boost::uuids::uuid adminAccountId() const { return adminAccountId_; }
    void setAdminAccountId(const boost::uuids::uuid& id) { adminAccountId_ = id; }

private:
    void setupPages();

    ClientManager* clientManager_;
    std::vector<BootstrapBundleInfo> bundles_;
    QString adminUsername_;
    QString adminEmail_;
    QString adminPassword_;
    QString selectedBundleCode_;
    boost::uuids::uuid adminAccountId_;
};

// Forward declarations of page classes
class WelcomePage;
class AdminAccountPage;
class BundleSelectionPage;
class ApplyProvisioningPage;

/**
 * @brief Welcome page explaining bootstrap mode and system initialization.
 */
class WelcomePage final : public QWizardPage {
    Q_OBJECT

public:
    explicit WelcomePage(SystemProvisionerWizard* wizard);

private:
    void setupUI();

    SystemProvisionerWizard* wizard_;
};

/**
 * @brief Page for creating the initial administrator account.
 */
class AdminAccountPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit AdminAccountPage(SystemProvisionerWizard* wizard);
    bool validatePage() override;

private slots:
    void onShowPasswordToggled(bool checked);
    void onPasswordChanged();

private:
    void setupUI();
    void updatePasswordMatchIndicator();

    SystemProvisionerWizard* wizard_;
    QLineEdit* usernameEdit_;
    QLineEdit* emailEdit_;
    QLineEdit* passwordEdit_;
    QLineEdit* confirmPasswordEdit_;
    QCheckBox* showPasswordCheckbox_;
    QLabel* passwordMatchLabel_;
    QLabel* validationLabel_;
};

/**
 * @brief Page for selecting a dataset bundle to provision.
 */
class BundleSelectionPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit BundleSelectionPage(SystemProvisionerWizard* wizard);
    void initializePage() override;
    bool validatePage() override;

private slots:
    void onBundleChanged(int index);

private:
    void setupUI();
    void populateBundles();

    SystemProvisionerWizard* wizard_;
    QComboBox* bundleCombo_;
    QLabel* descriptionLabel_;
};

/**
 * @brief Page for applying provisioning and showing progress.
 */
class ApplyProvisioningPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.apply_provisioning_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ApplyProvisioningPage(SystemProvisionerWizard* wizard);
    void initializePage() override;
    bool isComplete() const override;

private slots:
    void onProvisioningResult(const ProvisioningResult& result);

private:
    void startProvisioning();
    ProvisioningResult performProvisioning();
    void appendLog(const QString& message);
    void setStatus(const QString& status);

    SystemProvisionerWizard* wizard_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    QTextEdit* logOutput_;
    bool provisioningComplete_ = false;
    bool provisioningSuccess_ = false;
};

}

#endif
