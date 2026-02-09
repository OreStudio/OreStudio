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
#include <QTextEdit>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

class LeiEntityPicker;

/**
 * @brief Wizard for onboarding a new tenant.
 *
 * Three-page wizard that guides super admins through:
 * 1. TenantDetails - configure tenant code, name, type, hostname
 * 2. LeiPartyConfig - optionally select a GLEIF LEI entity (GLEIF mode only)
 * 3. Apply - provision the tenant and optionally publish LEI parties
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
        Page_TenantDetails,
        Page_LeiPartyConfig,
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

    QString leiDatasetSize() const { return leiDatasetSize_; }
    void setLeiDatasetSize(const QString& size) { leiDatasetSize_ = size; }

signals:
    /**
     * @brief Emitted when tenant onboarding completes successfully.
     *
     * @param tenantName The name of the onboarded tenant
     */
    void onboardingCompleted(const QString& tenantName);

private:
    void setupPages();

    ClientManager* clientManager_;
    bool gleifMode_ = false;
    QString tenantCode_;
    QString tenantName_;
    QString tenantType_ = QStringLiteral("organisation");
    QString tenantHostname_;
    QString tenantDescription_;
    QString rootLei_;
    QString rootLeiName_;
    QString leiDatasetSize_ = QStringLiteral("large");
};

// Forward declarations of page classes
class TenantDetailsPage;
class OnboardingLeiPartyConfigPage;
class ApplyOnboardingPage;

/**
 * @brief Page for configuring tenant details.
 *
 * Allows choosing between Blank and GLEIF-based modes and entering
 * tenant code, name, type, hostname, and description.
 */
class TenantDetailsPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit TenantDetailsPage(TenantOnboardingWizard* wizard);
    bool validatePage() override;
    int nextId() const override;

private slots:
    void onModeChanged();
    void onCodeChanged(const QString& text);

private:
    void setupUI();
    void updateHostname();

    TenantOnboardingWizard* wizard_;
    QRadioButton* blankRadio_;
    QRadioButton* gleifRadio_;
    QLineEdit* codeEdit_;
    QLineEdit* nameEdit_;
    QComboBox* typeCombo_;
    QLineEdit* hostnameEdit_;
    QLineEdit* descriptionEdit_;
    QComboBox* datasetSizeCombo_;
    QLabel* validationLabel_;
    bool hostnameManuallyEdited_ = false;
};

/**
 * @brief Page for selecting a GLEIF LEI entity.
 *
 * Only shown in GLEIF mode. Embeds a LeiEntityPicker widget
 * for searching and selecting a root LEI entity.
 */
class OnboardingLeiPartyConfigPage final : public QWizardPage {
    Q_OBJECT

public:
    explicit OnboardingLeiPartyConfigPage(TenantOnboardingWizard* wizard);
    void initializePage() override;
    bool validatePage() override;
    int nextId() const override;

private:
    void setupUI();

    TenantOnboardingWizard* wizard_;
    LeiEntityPicker* leiPicker_;
    QLabel* selectedEntityLabel_;
    bool leiLoaded_ = false;
};

/**
 * @brief Page that provisions the tenant and optionally publishes LEI data.
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
