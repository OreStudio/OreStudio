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
#include "ores.qt/TenantOnboardingWizard.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QGroupBox>
#include <QButtonGroup>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/LeiEntityPicker.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"
#include "ores.dq/messaging/publish_bundle_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

// ============================================================================
// TenantOnboardingWizard (Main Wizard)
// ============================================================================

TenantOnboardingWizard::TenantOnboardingWizard(
    ClientManager* clientManager,
    QWidget* parent)
    : QWizard(parent),
      clientManager_(clientManager) {

    setWindowTitle(tr("Onboard Tenant"));
    setMinimumSize(700, 550);
    resize(800, 600);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    setupPages();
}

void TenantOnboardingWizard::setupPages() {
    setPage(Page_TenantDetails, new TenantDetailsPage(this));
    setPage(Page_LeiPartyConfig, new OnboardingLeiPartyConfigPage(this));
    setPage(Page_Apply, new ApplyOnboardingPage(this));

    setStartId(Page_TenantDetails);
}

// ============================================================================
// TenantDetailsPage
// ============================================================================

TenantDetailsPage::TenantDetailsPage(TenantOnboardingWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Tenant Details"));
    setSubTitle(tr("Configure the new tenant. Choose whether to create a "
                   "blank tenant or seed it with GLEIF LEI data."));
    setupUI();
}

void TenantDetailsPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    // Mode selection
    auto* modeGroup = new QGroupBox(tr("Tenant Mode"), this);
    auto* modeLayout = new QVBoxLayout(modeGroup);

    blankRadio_ = new QRadioButton(
        tr("Blank Tenant - Create an empty tenant"), this);
    gleifRadio_ = new QRadioButton(
        tr("GLEIF-Based Tenant - Seed with LEI party data"), this);
    blankRadio_->setChecked(true);

    auto* buttonGroup = new QButtonGroup(this);
    buttonGroup->addButton(blankRadio_);
    buttonGroup->addButton(gleifRadio_);

    modeLayout->addWidget(blankRadio_);
    modeLayout->addWidget(gleifRadio_);
    layout->addWidget(modeGroup);

    connect(blankRadio_, &QRadioButton::toggled, this,
            &TenantDetailsPage::onModeChanged);

    // Tenant details form
    auto* formLayout = new QFormLayout();
    formLayout->setSpacing(10);

    codeEdit_ = new QLineEdit(this);
    codeEdit_->setPlaceholderText(tr("Unique tenant code (e.g., acme)"));
    codeEdit_->setMaxLength(100);
    formLayout->addRow(tr("Code:"), codeEdit_);

    nameEdit_ = new QLineEdit(this);
    nameEdit_->setPlaceholderText(tr("Display name (e.g., ACME Corporation)"));
    nameEdit_->setMaxLength(255);
    formLayout->addRow(tr("Name:"), nameEdit_);

    typeCombo_ = new QComboBox(this);
    typeCombo_->addItem(tr("Organisation"), "organisation");
    typeCombo_->addItem(tr("Individual"), "individual");
    typeCombo_->addItem(tr("Test"), "test");
    formLayout->addRow(tr("Type:"), typeCombo_);

    hostnameEdit_ = new QLineEdit(this);
    hostnameEdit_->setPlaceholderText(
        tr("Unique hostname (e.g., acme.localhost)"));
    hostnameEdit_->setMaxLength(255);
    formLayout->addRow(tr("Hostname:"), hostnameEdit_);

    descriptionEdit_ = new QLineEdit(this);
    descriptionEdit_->setPlaceholderText(tr("Optional description"));
    descriptionEdit_->setMaxLength(500);
    formLayout->addRow(tr("Description:"), descriptionEdit_);

    // Dataset size (only shown in GLEIF mode)
    datasetSizeCombo_ = new QComboBox(this);
    datasetSizeCombo_->addItem(tr("Large (full hierarchy)"), "large");
    datasetSizeCombo_->addItem(tr("Small (direct children only)"), "small");
    formLayout->addRow(tr("Dataset Size:"), datasetSizeCombo_);
    datasetSizeCombo_->setVisible(false);

    layout->addLayout(formLayout);

    // Validation label
    validationLabel_ = new QLabel(this);
    validationLabel_->setWordWrap(true);
    validationLabel_->setStyleSheet("QLabel { color: #cc0000; }");
    layout->addWidget(validationLabel_);

    layout->addStretch();

    // Auto-generate hostname from code
    connect(codeEdit_, &QLineEdit::textChanged,
            this, &TenantDetailsPage::onCodeChanged);

    // Track manual hostname edits
    connect(hostnameEdit_, &QLineEdit::textEdited, this, [this]() {
        hostnameManuallyEdited_ = true;
    });

    // Register mandatory fields
    registerField("tenantCode*", codeEdit_);
    registerField("tenantName*", nameEdit_);
}

void TenantDetailsPage::onModeChanged() {
    const bool gleif = gleifRadio_->isChecked();
    datasetSizeCombo_->setVisible(gleif);

    // Find the label for datasetSizeCombo_ and toggle it too
    auto* formLayout = qobject_cast<QFormLayout*>(
        datasetSizeCombo_->parentWidget()->layout());
    if (formLayout) {
        auto* label = formLayout->labelForField(datasetSizeCombo_);
        if (label) label->setVisible(gleif);
    }
}

void TenantDetailsPage::onCodeChanged(const QString& text) {
    updateHostname();
    Q_UNUSED(text);
}

void TenantDetailsPage::updateHostname() {
    if (hostnameManuallyEdited_)
        return;

    const QString code = codeEdit_->text().trimmed().toLower();
    if (!code.isEmpty()) {
        hostnameEdit_->setText(code + ".localhost");
    } else {
        hostnameEdit_->clear();
    }
}

bool TenantDetailsPage::validatePage() {
    validationLabel_->clear();

    const QString code = codeEdit_->text().trimmed();
    const QString name = nameEdit_->text().trimmed();
    const QString hostname = hostnameEdit_->text().trimmed();

    if (code.isEmpty()) {
        validationLabel_->setText(tr("Tenant code is required."));
        codeEdit_->setFocus();
        return false;
    }

    if (name.isEmpty()) {
        validationLabel_->setText(tr("Tenant name is required."));
        nameEdit_->setFocus();
        return false;
    }

    if (hostname.isEmpty()) {
        validationLabel_->setText(tr("Hostname is required."));
        hostnameEdit_->setFocus();
        return false;
    }

    // Store values in wizard
    wizard_->setGleifMode(gleifRadio_->isChecked());
    wizard_->setTenantCode(code);
    wizard_->setTenantName(name);
    wizard_->setTenantType(
        typeCombo_->currentData().toString());
    wizard_->setTenantHostname(hostname);
    wizard_->setTenantDescription(descriptionEdit_->text().trimmed());
    wizard_->setLeiDatasetSize(
        datasetSizeCombo_->currentData().toString());

    return true;
}

int TenantDetailsPage::nextId() const {
    // Skip LEI page if in blank mode
    if (blankRadio_->isChecked()) {
        return TenantOnboardingWizard::Page_Apply;
    }
    return TenantOnboardingWizard::Page_LeiPartyConfig;
}

// ============================================================================
// OnboardingLeiPartyConfigPage
// ============================================================================

OnboardingLeiPartyConfigPage::OnboardingLeiPartyConfigPage(
    TenantOnboardingWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Select LEI Entity"));
    setSubTitle(tr("Choose the root GLEIF LEI entity whose subsidiaries will "
                   "be imported as parties into the new tenant."));
    setupUI();
}

void OnboardingLeiPartyConfigPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    leiPicker_ = new LeiEntityPicker(wizard_->clientManager(), this);
    layout->addWidget(leiPicker_);

    selectedEntityLabel_ = new QLabel(this);
    selectedEntityLabel_->setWordWrap(true);
    selectedEntityLabel_->setStyleSheet("font-weight: bold;");
    layout->addWidget(selectedEntityLabel_);

    connect(leiPicker_, &LeiEntityPicker::entitySelected,
            this, [this](const QString& lei, const QString& name) {
        selectedEntityLabel_->setText(
            tr("Selected: %1 (%2)").arg(name, lei));
        emit completeChanged();
    });

    connect(leiPicker_, &LeiEntityPicker::selectionCleared,
            this, [this]() {
        selectedEntityLabel_->clear();
        emit completeChanged();
    });
}

void OnboardingLeiPartyConfigPage::initializePage() {
    if (!leiLoaded_) {
        leiPicker_->load();
        leiLoaded_ = true;
    }
}

bool OnboardingLeiPartyConfigPage::validatePage() {
    if (!leiPicker_->hasSelection()) {
        QMessageBox::warning(this, tr("No Selection"),
            tr("Please select an LEI entity."));
        return false;
    }

    wizard_->setRootLei(leiPicker_->selectedLei());
    wizard_->setRootLeiName(leiPicker_->selectedName());
    return true;
}

int OnboardingLeiPartyConfigPage::nextId() const {
    // Skip this page if in blank mode
    if (!wizard_->isGleifMode()) {
        return TenantOnboardingWizard::Page_Apply;
    }
    return TenantOnboardingWizard::Page_Apply;
}

// ============================================================================
// ApplyOnboardingPage
// ============================================================================

ApplyOnboardingPage::ApplyOnboardingPage(TenantOnboardingWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Provisioning Tenant"));
    setFinalPage(true);

    auto* layout = new QVBoxLayout(this);

    statusLabel_ = new QLabel(tr("Starting..."), this);
    statusLabel_->setStyleSheet("font-weight: bold;");
    layout->addWidget(statusLabel_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0); // indeterminate
    layout->addWidget(progressBar_);

    logOutput_ = new QTextEdit(this);
    logOutput_->setReadOnly(true);
    logOutput_->setFont(QFont("monospace", 9));
    layout->addWidget(logOutput_);
}

bool ApplyOnboardingPage::isComplete() const {
    return onboardingComplete_;
}

void ApplyOnboardingPage::initializePage() {
    onboardingComplete_ = false;
    onboardingSuccess_ = false;
    logOutput_->clear();
    statusLabel_->setText(tr("Provisioning tenant..."));
    progressBar_->setRange(0, 0);

    startOnboarding();
}

void ApplyOnboardingPage::appendLog(const QString& message) {
    logOutput_->append(message);
    // Scroll to bottom
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void ApplyOnboardingPage::startOnboarding() {
    const std::string code = wizard_->tenantCode().toStdString();
    const std::string name = wizard_->tenantName().toStdString();
    const std::string type = wizard_->tenantType().toStdString();
    const std::string hostname = wizard_->tenantHostname().toStdString();
    const std::string description = wizard_->tenantDescription().toStdString();
    const bool gleifMode = wizard_->isGleifMode();
    const std::string rootLei = wizard_->rootLei().toStdString();
    const std::string datasetSize = wizard_->leiDatasetSize().toStdString();
    ClientManager* clientManager = wizard_->clientManager();

    struct OnboardingResult {
        bool provisionSuccess = false;
        std::string provisionError;
        std::string tenantId;

        bool publishSuccess = false;
        std::string publishError;
        std::uint32_t datasetsProcessed = 0;
        std::uint32_t datasetsSucceeded = 0;
    };

    auto* watcher = new QFutureWatcher<OnboardingResult>(this);
    connect(watcher, &QFutureWatcher<OnboardingResult>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        if (!result.provisionSuccess) {
            statusLabel_->setText(tr("Provisioning failed"));
            appendLog(tr("ERROR: %1").arg(
                QString::fromStdString(result.provisionError)));
            progressBar_->setRange(0, 1);
            progressBar_->setValue(0);
        } else if (wizard_->isGleifMode() && !result.publishSuccess) {
            statusLabel_->setText(tr("Tenant created but data publication failed"));
            appendLog(tr("Tenant created (ID: %1)").arg(
                QString::fromStdString(result.tenantId)));
            appendLog(tr("WARNING: LEI data publication failed: %1").arg(
                QString::fromStdString(result.publishError)));
            progressBar_->setRange(0, 1);
            progressBar_->setValue(1);
        } else {
            statusLabel_->setText(tr("Onboarding complete"));
            appendLog(tr("Tenant '%1' onboarded successfully.").arg(
                wizard_->tenantName()));
            if (wizard_->isGleifMode()) {
                appendLog(tr("Published %1 datasets (%2 succeeded).")
                    .arg(result.datasetsProcessed)
                    .arg(result.datasetsSucceeded));
            }
            progressBar_->setRange(0, 1);
            progressBar_->setValue(1);
            onboardingSuccess_ = true;
            emit wizard_->onboardingCompleted(wizard_->tenantName());
        }

        onboardingComplete_ = true;
        emit completeChanged();
    });

    QFuture<OnboardingResult> future = QtConcurrent::run(
        [clientManager, code, name, type, hostname, description,
         gleifMode, rootLei, datasetSize]() -> OnboardingResult {

            OnboardingResult result;

            // Step 1: Provision the tenant
            iam::messaging::provision_tenant_request provRequest;
            provRequest.type = type;
            provRequest.code = code;
            provRequest.name = name;
            provRequest.hostname = hostname;
            provRequest.description = description;

            auto provResult = clientManager->process_authenticated_request(
                std::move(provRequest));

            if (!provResult) {
                result.provisionError = "Failed to communicate with server";
                return result;
            }

            if (!provResult->success) {
                result.provisionError = provResult->error_message;
                return result;
            }

            result.provisionSuccess = true;
            result.tenantId = provResult->tenant_id;

            // Step 2: If GLEIF mode, publish lei_parties bundle to the new tenant
            if (gleifMode && !rootLei.empty()) {
                dq::messaging::publish_bundle_request pubRequest;
                pubRequest.bundle_code = "lei_parties";
                pubRequest.mode = dq::domain::publication_mode::upsert;
                pubRequest.published_by = clientManager->currentUsername();
                pubRequest.atomic = true;
                pubRequest.target_tenant_id = result.tenantId;

                // Build params_json with root LEI and dataset size
                pubRequest.params_json =
                    "{\"lei_parties\":{\"root_lei\":\"" + rootLei + "\"},"
                    "\"lei_dataset_size\":\"" + datasetSize + "\"}";

                auto pubResult = clientManager->process_authenticated_request(
                    std::move(pubRequest));

                if (!pubResult) {
                    result.publishError =
                        "Failed to communicate with server for data publication";
                    return result;
                }

                if (!pubResult->success) {
                    result.publishError = pubResult->error_message;
                    return result;
                }

                result.publishSuccess = true;
                result.datasetsProcessed = pubResult->datasets_processed;
                result.datasetsSucceeded = pubResult->datasets_succeeded;
            } else {
                // Blank mode, no publication needed
                result.publishSuccess = true;
            }

            return result;
        }
    );

    watcher->setFuture(future);

    appendLog(tr("Provisioning tenant '%1' (code: %2, hostname: %3)...")
        .arg(wizard_->tenantName(), wizard_->tenantCode(),
             wizard_->tenantHostname()));
    if (gleifMode) {
        appendLog(tr("Will publish LEI parties data (root: %1, size: %2)")
            .arg(wizard_->rootLeiName(), wizard_->leiDatasetSize()));
    }
}

}
