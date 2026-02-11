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
#ifndef ORES_QT_TENANT_ONBOARDING_WIZARD_HPP
#define ORES_QT_TENANT_ONBOARDING_WIZARD_HPP

#include <QWizard>
#include <QWizardPage>
#include <QLineEdit>
#include <QLabel>
#include <QComboBox>
#include <QRadioButton>
#include <QProgressBar>
#include <QCheckBox>
#include <QTextEdit>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

class LeiEntityPicker;

/**
 * @brief Wizard for onboarding a new tenant.
 *
 * Four-page wizard that guides super admins through:
 * 1. ModeAndLei - choose Blank or GLEIF mode; in GLEIF mode, select root LEI
 *    entity whose name will seed the tenant details
 * 2. TenantDetails - configure tenant code, name, type, hostname (pre-filled
 *    from LEI entity in GLEIF mode)
 * 3. AdminAccount - create the initial admin account for the new tenant
 * 4. Apply - provision the tenant and create admin account
 *
 * This wizard is accessible from System > Identity > Onboard Tenant and
 * from the TenantMdiWindow toolbar.
 */
class TenantOnboardingWizard final : public QWizard {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.tenant_onboarding_wizard";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum PageId {
        Page_ModeAndLei,
        Page_TenantDetails,
        Page_AdminAccount,
        Page_Apply
    };

    explicit TenantOnboardingWizard(
        ClientManager* clientManager,
        QWidget* parent = nullptr);

    ~TenantOnboardingWizard() override = default;

    ClientManager* clientManager() const { return clientManager_; }

    bool isGleifMode() const { return gleifMode_; }
    void setGleifMode(bool gleif) { gleifMode_ = gleif; }

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

    QString rootLei() const { return rootLei_; }
    void setRootLei(const QString& lei) { rootLei_ = lei; }

    QString rootLeiName() const { return rootLeiName_; }
    void setRootLeiName(const QString& name) { rootLeiName_ = name; }

    QString adminUsername() const { return adminUsername_; }
    void setAdminUsername(const QString& u) { adminUsername_ = u; }

    QString adminPassword() const { return adminPassword_; }
    void setAdminPassword(const QString& p) { adminPassword_ = p; }

    QString adminEmail() const { return adminEmail_; }
    void setAdminEmail(const QString& e) { adminEmail_ = e; }

signals:
    void onboardingCompleted(const QString& tenantName);

private:
    void setupPages();

    ClientManager* clientManager_;
    bool gleifMode_ = false;
    QString tenantCode_;
    QString tenantName_;
    QString tenantType_ = QStringLiteral("evaluation");
    QString tenantHostname_;
    QString tenantDescription_;
    QString rootLei_;
    QString rootLeiName_;
    QString adminUsername_;
    QString adminPassword_;
    QString adminEmail_;
};

// Forward declarations of page classes
class ModeAndLeiPage;
class TenantDetailsPage;
class OnboardingAdminAccountPage;
class ApplyOnboardingPage;

/**
 * @brief First page: choose Blank or GLEIF mode and select LEI entity.
 *
 * In GLEIF mode, embeds a LeiEntityPicker for selecting the root LEI
 * entity whose legal name will pre-fill the tenant details on the next page.
 */
class ModeAndLeiPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit ModeAndLeiPage(TenantOnboardingWizard* wizard);
    void initializePage() override;
    bool validatePage() override;
    int nextId() const override;

private slots:
    void onModeChanged();

private:
    void setupUI();

    TenantOnboardingWizard* wizard_;
    QRadioButton* blankRadio_;
    QRadioButton* gleifRadio_;
    LeiEntityPicker* leiPicker_;
    QLabel* selectedEntityLabel_;
    bool leiLoaded_ = false;
};

/**
 * @brief Second page: configure tenant details.
 *
 * In GLEIF mode, Code and Name are pre-filled from the selected LEI entity
 * but remain editable. In Blank mode, all fields start empty.
 */
class TenantDetailsPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit TenantDetailsPage(TenantOnboardingWizard* wizard);
    void initializePage() override;
    bool validatePage() override;
    int nextId() const override;

private slots:
    void onCodeChanged(const QString& text);

private:
    void setupUI();
    void updateHostname();

    TenantOnboardingWizard* wizard_;
    QLineEdit* codeEdit_;
    QLineEdit* nameEdit_;
    QComboBox* typeCombo_;
    QLineEdit* hostnameEdit_;
    QLineEdit* descriptionEdit_;
    QLabel* validationLabel_;
    bool hostnameManuallyEdited_ = false;
};

/**
 * @brief Third page: create the initial admin account for the new tenant.
 */
class OnboardingAdminAccountPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit OnboardingAdminAccountPage(TenantOnboardingWizard* wizard);
    void initializePage() override;
    bool validatePage() override;
    int nextId() const override;

private slots:
    void onShowPasswordToggled(bool checked);

private:
    void setupUI();

    TenantOnboardingWizard* wizard_;
    QLineEdit* usernameEdit_;
    QLineEdit* emailEdit_;
    QLineEdit* passwordEdit_;
    QLineEdit* confirmPasswordEdit_;
    QCheckBox* showPasswordCheck_;
    QLabel* validationLabel_;
};

/**
 * @brief Fourth page: provisions the tenant and creates admin account.
 *
 * Runs the provisioning workflow asynchronously with progress output.
 */
class ApplyOnboardingPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.apply_onboarding_page";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ApplyOnboardingPage(TenantOnboardingWizard* wizard);
    void initializePage() override;
    bool isComplete() const override;

private:
    void startOnboarding();
    void appendLog(const QString& message);

    TenantOnboardingWizard* wizard_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    QTextEdit* logOutput_;
    bool onboardingComplete_ = false;
    bool onboardingSuccess_ = false;
};

}

#endif
