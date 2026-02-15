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

#include <QWizard>
#include <QWizardPage>
#include <QLineEdit>
#include <QLabel>
#include <QCheckBox>
#include <QComboBox>
#include <QRadioButton>
#include <QProgressBar>
#include <QTextEdit>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

/**
 * @brief Wizard for initial system bootstrap and first tenant creation.
 *
 * Seven-page wizard that guides users through:
 * 1. Welcome - explains bootstrap mode
 * 2. Create Administrator Account - sets up the first admin
 * 3. Setup Mode - choose single-tenant or multi-tenant
 * 4. Tenant Details - configure the first tenant
 * 5. Tenant Admin - create the tenant admin account
 * 6. Apply - provision the first tenant asynchronously
 * 7. Complete - confirms success and shows next steps
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
    enum PageId {
        Page_Welcome,
        Page_AdminAccount,
        Page_SetupMode,
        Page_TenantDetails,
        Page_TenantAdmin,
        Page_Apply,
        Page_Complete
    };

    explicit SystemProvisionerWizard(
        ClientManager* clientManager,
        QWidget* parent = nullptr);

    ~SystemProvisionerWizard() override = default;

    ClientManager* clientManager() const { return clientManager_; }

    // Admin account accessors
    QString adminUsername() const { return adminUsername_; }
    QString adminEmail() const { return adminEmail_; }
    QString adminPassword() const { return adminPassword_; }
    void setAdminCredentials(const QString& username, const QString& email,
                             const QString& password);

    boost::uuids::uuid adminAccountId() const { return adminAccountId_; }
    void setAdminAccountId(const boost::uuids::uuid& id) { adminAccountId_ = id; }

    // Setup mode accessors
    bool isMultiTenantMode() const { return multiTenantMode_; }
    void setMultiTenantMode(bool multi) { multiTenantMode_ = multi; }

    // Tenant details accessors
    QString tenantCode() const { return tenantCode_; }
    void setTenantCode(const QString& code) { tenantCode_ = code; }

    QString tenantName() const { return tenantName_; }
    void setTenantName(const QString& name) { tenantName_ = name; }

    QString tenantType() const { return tenantType_; }
    void setTenantType(const QString& type) { tenantType_ = type; }

    QString tenantHostname() const { return tenantHostname_; }
    void setTenantHostname(const QString& hostname) { tenantHostname_ = hostname; }

    QString tenantDescription() const { return tenantDescription_; }
    void setTenantDescription(const QString& desc) { tenantDescription_ = desc; }

    // Tenant admin accessors
    QString tenantAdminUsername() const { return tenantAdminUsername_; }
    void setTenantAdminUsername(const QString& u) { tenantAdminUsername_ = u; }

    QString tenantAdminPassword() const { return tenantAdminPassword_; }
    void setTenantAdminPassword(const QString& p) { tenantAdminPassword_ = p; }

    QString tenantAdminEmail() const { return tenantAdminEmail_; }
    void setTenantAdminEmail(const QString& e) { tenantAdminEmail_ = e; }

    QString provisionedTenantId() const { return provisionedTenantId_; }
    void setProvisionedTenantId(const QString& id) { provisionedTenantId_ = id; }

private:
    void setupPages();

    ClientManager* clientManager_;
    QString adminUsername_;
    QString adminEmail_;
    QString adminPassword_;
    boost::uuids::uuid adminAccountId_;
    bool multiTenantMode_ = false;
    QString tenantCode_;
    QString tenantName_;
    QString tenantType_ = QStringLiteral("evaluation");
    QString tenantHostname_;
    QString tenantDescription_;
    QString tenantAdminUsername_;
    QString tenantAdminPassword_;
    QString tenantAdminEmail_;
    QString provisionedTenantId_;
};

// Forward declarations of page classes
class WelcomePage;
class AdminAccountPage;
class SetupModePage;
class ProvisionerTenantDetailsPage;
class ProvisionerTenantAdminPage;
class ProvisionerApplyPage;
class ProvisionerCompletePage;

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

private:
    inline static std::string_view logger_name =
        "ores.qt.admin_account_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit AdminAccountPage(SystemProvisionerWizard* wizard);
    void initializePage() override;
    bool validatePage() override;
    int nextId() const override;

private slots:
    void onShowPasswordToggled(bool checked);

private:
    void setupUI();

    SystemProvisionerWizard* wizard_;
    QLineEdit* usernameEdit_;
    QLineEdit* emailEdit_;
    QLineEdit* passwordEdit_;
    QLineEdit* confirmPasswordEdit_;
    QCheckBox* showPasswordCheckbox_;
    QLabel* validationLabel_;
    bool accountCreated_ = false;
};

/**
 * @brief Page for choosing single-tenant or multi-tenant setup mode.
 */
class SetupModePage final : public QWizardPage {
    Q_OBJECT

public:
    explicit SetupModePage(SystemProvisionerWizard* wizard);
    void initializePage() override;
    bool validatePage() override;

private:
    void setupUI();

    SystemProvisionerWizard* wizard_;
    QRadioButton* singleTenantRadio_;
    QRadioButton* multiTenantRadio_;
};

/**
 * @brief Page for configuring the first tenant's details.
 */
class ProvisionerTenantDetailsPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit ProvisionerTenantDetailsPage(SystemProvisionerWizard* wizard);
    void initializePage() override;
    bool validatePage() override;

private slots:
    void onCodeChanged(const QString& text);

private:
    void setupUI();

    SystemProvisionerWizard* wizard_;
    QLineEdit* codeEdit_;
    QLineEdit* nameEdit_;
    QComboBox* typeCombo_;
    QLineEdit* hostnameEdit_;
    QLineEdit* descriptionEdit_;
    QLabel* validationLabel_;
    bool hostnameManuallyEdited_ = false;
};

/**
 * @brief Page for creating the first tenant's admin account.
 */
class ProvisionerTenantAdminPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit ProvisionerTenantAdminPage(SystemProvisionerWizard* wizard);
    void initializePage() override;
    bool validatePage() override;

private slots:
    void onShowPasswordToggled(bool checked);

private:
    void setupUI();

    SystemProvisionerWizard* wizard_;
    QLineEdit* usernameEdit_;
    QLineEdit* emailEdit_;
    QLineEdit* passwordEdit_;
    QLineEdit* confirmPasswordEdit_;
    QCheckBox* showPasswordCheck_;
    QLabel* validationLabel_;
};

/**
 * @brief Page for async tenant provisioning execution.
 */
class ProvisionerApplyPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.provisioner_apply_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ProvisionerApplyPage(SystemProvisionerWizard* wizard);
    void initializePage() override;
    bool isComplete() const override;

private:
    void startProvisioning();
    void appendLog(const QString& message);

    SystemProvisionerWizard* wizard_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    QTextEdit* logOutput_;
    bool provisioningComplete_ = false;
    bool provisioningSuccess_ = false;
};

/**
 * @brief Final page showing bootstrap completion summary.
 */
class ProvisionerCompletePage final : public QWizardPage {
    Q_OBJECT

public:
    explicit ProvisionerCompletePage(SystemProvisionerWizard* wizard);
    void initializePage() override;

private:
    void setupUI();

    SystemProvisionerWizard* wizard_;
    QLabel* summaryLabel_;
};

}

#endif
