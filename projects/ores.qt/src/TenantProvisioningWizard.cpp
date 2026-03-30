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
#include "ores.qt/TenantProvisioningWizard.hpp"
#include "ores.qt/ClientDatasetBundleModel.hpp"
#include "ores.qt/FontUtils.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WidgetUtils.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGroupBox>
#include <QSizePolicy>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.database/domain/change_reason_constants.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.variability.api/domain/system_setting.hpp"
#include "ores.variability.api/messaging/system_settings_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;
namespace reason = ores::database::domain::change_reason_constants;

// ============================================================================
// TenantProvisioningWizard
// ============================================================================

TenantProvisioningWizard::TenantProvisioningWizard(
    ClientManager* clientManager,
    QWidget* parent)
    : QWizard(parent),
      clientManager_(clientManager) {

    setWindowTitle(tr("New Tenant Provisioner"));
    setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::BuildingSkyscraper, IconUtils::DefaultIconColor));
    setMinimumSize(900, 700);
    resize(900, 700);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage, true);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    setupPages();

    // Clear bootstrap flag on cancel/reject too
    connect(this, &QWizard::rejected, this, [this]() {
        clearBootstrapFlag();
    });
}

void TenantProvisioningWizard::setupPages() {
    WidgetUtils::setupComboBoxes(this);
    setPage(Page_Welcome, new ProvisioningWelcomePage(this));
    setPage(Page_BundleSelection, new BundleSelectionPage(this));
    setPage(Page_BundleInstall, new BundleInstallPage(this));
    setPage(Page_AccountSetup, new AccountSetupPage(this));
    setPage(Page_Summary, new TenantApplyAndSummaryPage(this));

    setStartId(Page_Welcome);
}

void TenantProvisioningWizard::clearBootstrapFlag() {
    BOOST_LOG_SEV(lg(), info) << "Clearing tenant bootstrap mode flag";

    variability::domain::system_setting setting;
    setting.name = "system.bootstrap_mode";
    setting.value = "false";
    setting.data_type = "boolean";
    setting.description = "Bootstrap mode disabled after tenant setup";
    setting.modified_by = clientManager_->currentUsername();
    setting.change_reason_code = std::string(reason::codes::new_record);
    setting.change_commentary = "Tenant setup wizard completed";
    variability::messaging::save_setting_request req;
    req.data = std::move(setting);

    auto result = clientManager_->process_authenticated_request(std::move(req));
    if (!result) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to clear bootstrap flag: "
                                  << "no response from server";
    } else if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to clear bootstrap flag: "
                                  << (result->message.empty() ? "Unknown error" : result->message);
    } else {
        BOOST_LOG_SEV(lg(), info) << "Bootstrap flag cleared successfully";
    }
}

// ============================================================================
// ProvisioningWelcomePage
// ============================================================================

ProvisioningWelcomePage::ProvisioningWelcomePage(
    TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Welcome"));
    setupUI();
}

void ProvisioningWelcomePage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* welcomeLabel = new QLabel(
        tr("Your tenant is new and needs initial setup."), this);
    welcomeLabel->setStyleSheet("font-size: 16pt; font-weight: bold;");
    welcomeLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(welcomeLabel);

    layout->addSpacing(10);

    auto* descLabel = new QLabel(this);
    descLabel->setWordWrap(true);
    descLabel->setText(
        tr("This wizard will help you set up your tenant with the essential "
           "reference data needed to get started, and create the first "
           "operational account.\n\n"
           "The setup process includes:"));
    layout->addWidget(descLabel);

    auto* stepsLabel = new QLabel(this);
    stepsLabel->setWordWrap(true);
    stepsLabel->setTextFormat(Qt::RichText);
    stepsLabel->setText(
        tr("<ol>"
           "<li><b>Select Catalogue</b> - Choose a pre-configured "
           "set of reference data (currencies, countries, etc.).</li>"
           "<li><b>Publish Catalogue</b> - Publish the selected data to your "
           "tenant.</li>"
           "<li><b>Create Account</b> - Create the first party admin account. "
           "This account will complete the party setup on first login.</li>"
           "</ol>"));
    layout->addWidget(stepsLabel);

    layout->addStretch();

    auto* noteBox = new QGroupBox(tr("Note"), this);
    auto* noteLayout = new QVBoxLayout(noteBox);
    auto* noteLabel = new QLabel(
        tr("You can skip this setup by clicking Cancel. The wizard will not "
           "appear again, and you can set up reference data manually using "
           "the Data Librarian."),
        this);
    noteLabel->setWordWrap(true);
    noteLayout->addWidget(noteLabel);
    layout->addWidget(noteBox);
}

// ============================================================================
// BundleSelectionPage
// ============================================================================

BundleSelectionPage::BundleSelectionPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Select Catalogue"));
    setSubTitle(tr("Choose a catalogue of reference data to publish to your "
                   "tenant. Each catalogue contains a pre-configured set of "
                   "currencies, countries, and other reference data."));

    setupUI();
}

void BundleSelectionPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    bundleModel_ = new ClientDatasetBundleModel(
        wizard_->clientManager(), this);

    auto* comboLabel = new QLabel(tr("Catalogue:"), this);
    layout->addWidget(comboLabel);

    bundleCombo_ = new QComboBox(this);
    bundleCombo_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
    layout->addWidget(bundleCombo_);

    layout->addSpacing(12);

    descriptionLabel_ = new QLabel(this);
    descriptionLabel_->setWordWrap(true);
    descriptionLabel_->setTextFormat(Qt::PlainText);
    descriptionLabel_->setFrameShape(QFrame::StyledPanel);
    descriptionLabel_->setContentsMargins(8, 8, 8, 8);
    descriptionLabel_->setMinimumHeight(60);
    layout->addWidget(descriptionLabel_);

    layout->addStretch();

    statusLabel_ = new QLabel(this);
    statusLabel_->setWordWrap(true);
    layout->addWidget(statusLabel_);

    connect(bundleCombo_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &BundleSelectionPage::onBundleChanged);

    // Populate combo when data arrives
    connect(bundleModel_, &ClientDatasetBundleModel::dataLoaded,
            this, [this]() {
        bundleCombo_->clear();
        const int count = bundleModel_->rowCount();
        for (int i = 0; i < count; ++i) {
            const auto* bundle = bundleModel_->getBundle(i);
            if (bundle) {
                bundleCombo_->addItem(
                    QString::fromStdString(bundle->name), i);
            }
        }
        if (count > 0) {
            bundleCombo_->setCurrentIndex(0);
            statusLabel_->clear();
        } else {
            statusLabel_->setText(tr("No catalogues available."));
        }
        emit completeChanged();
    });

    connect(bundleModel_, &ClientDatasetBundleModel::loadError,
            this, [this](const QString& msg) {
        statusLabel_->setText(tr("Failed to load catalogues: %1").arg(msg));
    });

    WidgetUtils::setupComboBoxes(this);
}

void BundleSelectionPage::onBundleChanged(int index) {
    if (index < 0) {
        descriptionLabel_->clear();
        return;
    }

    const int row = bundleCombo_->itemData(index).toInt();
    const auto* bundle = bundleModel_->getBundle(row);
    if (bundle) {
        descriptionLabel_->setText(
            QString::fromStdString(bundle->description));
    } else {
        descriptionLabel_->clear();
    }
    emit completeChanged();
}

void BundleSelectionPage::initializePage() {
    bundleCombo_->clear();
    descriptionLabel_->clear();
    bundleModel_->refresh();
    statusLabel_->setText(tr("Loading available catalogues..."));
}

bool BundleSelectionPage::isComplete() const {
    return bundleCombo_->currentIndex() >= 0;
}

bool BundleSelectionPage::validatePage() {
    const int index = bundleCombo_->currentIndex();
    if (index < 0) {
        statusLabel_->setText(tr("Please select a catalogue to continue."));
        return false;
    }

    const int row = bundleCombo_->itemData(index).toInt();
    const auto* bundle = bundleModel_->getBundle(row);
    if (!bundle) {
        statusLabel_->setText(tr("Invalid selection."));
        return false;
    }

    wizard_->setSelectedBundleCode(QString::fromStdString(bundle->code));
    wizard_->setSelectedBundleName(QString::fromStdString(bundle->name));
    return true;
}

// ============================================================================
// BundleInstallPage
// ============================================================================

BundleInstallPage::BundleInstallPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Publishing Catalogue"));
    setFinalPage(false);

    auto* layout = new QVBoxLayout(this);

    statusLabel_ = new QLabel(tr("Starting..."), this);
    statusLabel_->setStyleSheet("font-weight: bold;");
    layout->addWidget(statusLabel_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0); // indeterminate
    progressBar_->setTextVisible(false);
    progressBar_->setStyleSheet(
        "QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
        "background: #2d2d2d; height: 20px; }"
        "QProgressBar::chunk { background-color: #4a9eff; }");
    layout->addWidget(progressBar_);

    logOutput_ = new QTextEdit(this);
    logOutput_->setReadOnly(true);
    logOutput_->setFont(FontUtils::monospace());
    layout->addWidget(logOutput_);
}

bool BundleInstallPage::isComplete() const {
    return publishComplete_;
}

void BundleInstallPage::initializePage() {
    publishComplete_ = false;
    publishSuccess_ = false;
    logOutput_->clear();
    statusLabel_->setText(tr("Publishing catalogue '%1'...").arg(
        wizard_->selectedBundleName()));
    progressBar_->setRange(0, 0);
    progressBar_->setStyleSheet(
        "QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
        "background: #2d2d2d; height: 20px; }"
        "QProgressBar::chunk { background-color: #4a9eff; }");

    startPublish();
}

void BundleInstallPage::appendLog(const QString& message) {
    logOutput_->append(message);
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void BundleInstallPage::startPublish() {
    const std::string bundleCode = wizard_->selectedBundleCode().toStdString();
    const std::string publishedBy = wizard_->clientManager()->currentUsername();
    ClientManager* clientManager = wizard_->clientManager();

    BOOST_LOG_SEV(lg(), info) << "Publishing bundle: " << bundleCode;

    using ResponseType = dq::messaging::publish_bundle_response;

    auto* watcher = new QFutureWatcher<std::optional<ResponseType>>(this);
    connect(watcher, &QFutureWatcher<std::optional<ResponseType>>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        if (!result) {
            BOOST_LOG_SEV(lg(), error) << "Bundle publication: no server response";
            statusLabel_->setText(tr("Publication failed!"));
            appendLog(tr("ERROR: Failed to communicate with server."));
            progressBar_->setStyleSheet(
                "QProgressBar::chunk { background-color: #cc0000; }");
            publishSuccess_ = false;
        } else if (!result->success) {
            BOOST_LOG_SEV(lg(), error) << "Bundle publication failed: "
                                       << result->error_message;
            statusLabel_->setText(tr("Publication failed!"));
            appendLog(tr("ERROR: %1").arg(
                QString::fromStdString(result->error_message)));
            progressBar_->setStyleSheet(
                "QProgressBar::chunk { background-color: #cc0000; }");
            publishSuccess_ = false;
        } else {
            BOOST_LOG_SEV(lg(), info) << "Bundle publication succeeded: "
                << result->datasets_succeeded << " datasets";
            statusLabel_->setText(tr("Publication completed successfully!"));
            appendLog(tr("Published %1 datasets (%2 records inserted, %3 updated).")
                .arg(result->datasets_succeeded)
                .arg(result->total_records_inserted)
                .arg(result->total_records_updated));
            publishSuccess_ = true;
        }

        publishComplete_ = true;
        emit completeChanged();
    });

    QFuture<std::optional<ResponseType>> future = QtConcurrent::run(
        [clientManager, bundleCode, publishedBy]() -> std::optional<ResponseType> {

            dq::messaging::publish_bundle_request request;
            request.bundle_code = bundleCode;
            request.mode = dq::domain::publication_mode::upsert;
            request.published_by = publishedBy;
            request.atomic = true;

            auto result = clientManager->process_authenticated_request(
                std::move(request));

            if (!result) {
                return std::nullopt;
            }
            return *result;
        }
    );

    watcher->setFuture(future);

    appendLog(tr("Publishing catalogue '%1'...")
        .arg(wizard_->selectedBundleName()));
}

// ============================================================================
// AccountSetupPage
// ============================================================================

AccountSetupPage::AccountSetupPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Create Party Admin Account"));
    setSubTitle(tr("Create the first operational account for this tenant. "
                   "This account will complete party setup on first login."));

    setupUI();
}

void AccountSetupPage::setupUI() {
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(10);

    auto* infoLabel = new QLabel(
        tr("This account will be the first party administrator. "
           "After this wizard completes, log in with these credentials "
           "to run the party setup wizard."),
        this);
    infoLabel->setWordWrap(true);
    layout->addWidget(infoLabel);

    layout->addSpacing(8);

    auto* formLayout = new QHBoxLayout();
    auto* labelCol = new QVBoxLayout();
    auto* fieldCol = new QVBoxLayout();

    labelCol->addWidget(new QLabel(tr("Username:"), this));
    labelCol->addWidget(new QLabel(tr("Password:"), this));
    labelCol->addWidget(new QLabel(tr("Confirm Password:"), this));
    labelCol->addWidget(new QLabel(tr("Email:"), this));
    labelCol->addStretch();

    usernameEdit_ = new QLineEdit(this);
    usernameEdit_->setPlaceholderText(tr("e.g. party_admin"));
    passwordEdit_ = new QLineEdit(this);
    passwordEdit_->setEchoMode(QLineEdit::Password);
    confirmPasswordEdit_ = new QLineEdit(this);
    confirmPasswordEdit_->setEchoMode(QLineEdit::Password);
    emailEdit_ = new QLineEdit(this);
    emailEdit_->setPlaceholderText(tr("e.g. admin@example.com"));

    fieldCol->addWidget(usernameEdit_);
    fieldCol->addWidget(passwordEdit_);
    fieldCol->addWidget(confirmPasswordEdit_);
    fieldCol->addWidget(emailEdit_);
    fieldCol->addStretch();

    formLayout->addLayout(labelCol);
    formLayout->addLayout(fieldCol);
    layout->addLayout(formLayout);

    layout->addStretch();

    statusLabel_ = new QLabel(this);
    statusLabel_->setWordWrap(true);
    layout->addWidget(statusLabel_);

    connect(usernameEdit_, &QLineEdit::textChanged,
            this, &AccountSetupPage::completeChanged);
    connect(passwordEdit_, &QLineEdit::textChanged,
            this, &AccountSetupPage::completeChanged);
    connect(confirmPasswordEdit_, &QLineEdit::textChanged,
            this, &AccountSetupPage::completeChanged);
}

bool AccountSetupPage::isComplete() const {
    return !usernameEdit_->text().trimmed().isEmpty()
        && !passwordEdit_->text().isEmpty()
        && passwordEdit_->text() == confirmPasswordEdit_->text();
}

bool AccountSetupPage::validatePage() {
    const std::string username = usernameEdit_->text().trimmed().toStdString();
    const std::string password = passwordEdit_->text().toStdString();
    const std::string email = emailEdit_->text().trimmed().toStdString();

    if (passwordEdit_->text() != confirmPasswordEdit_->text()) {
        statusLabel_->setText(tr("Passwords do not match."));
        return false;
    }

    statusLabel_->setText(tr("Creating account..."));
    ClientManager* clientManager = wizard_->clientManager();

    iam::messaging::save_account_request request;
    request.principal = username;
    request.password = password;
    request.email = email;
    request.totp_secret = "";

    auto response = clientManager->process_authenticated_request(
        std::move(request));

    if (!response) {
        statusLabel_->setText(tr("Failed to communicate with server."));
        BOOST_LOG_SEV(lg(), error) << "Account creation: no server response";
        return false;
    }

    if (!response->success) {
        statusLabel_->setText(
            tr("Failed to create account: %1")
                .arg(QString::fromStdString(response->message)));
        BOOST_LOG_SEV(lg(), error) << "Account creation failed: "
                                   << response->message;
        return false;
    }

    BOOST_LOG_SEV(lg(), info) << "Account created: " << username
                              << " (id: " << response->account_id << ")";
    statusLabel_->setText(tr("Account created successfully."));
    wizard_->setNewAccountUsername(
        QString::fromStdString(username));
    return true;
}

// ============================================================================
// TenantApplyAndSummaryPage
// ============================================================================

TenantApplyAndSummaryPage::TenantApplyAndSummaryPage(
    TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Setup Complete"));
    setFinalPage(true);

    setupUI();
}

void TenantApplyAndSummaryPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* headerLabel = new QLabel(
        tr("Tenant setup complete"), this);
    headerLabel->setStyleSheet("font-size: 16pt; font-weight: bold;");
    headerLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(headerLabel);

    layout->addSpacing(10);

    summaryLabel_ = new QLabel(this);
    summaryLabel_->setWordWrap(true);
    summaryLabel_->setTextFormat(Qt::RichText);
    summaryLabel_->setAlignment(Qt::AlignLeft | Qt::AlignTop);
    layout->addWidget(summaryLabel_);

    layout->addStretch();

    auto* nextStepsBox = new QGroupBox(tr("Next Steps"), this);
    auto* nextStepsLayout = new QVBoxLayout(nextStepsBox);
    auto* nextStepsLabel = new QLabel(
        tr("Log out and log in with the party admin account you just created. "
           "The party setup wizard will appear automatically to complete the "
           "operational setup of this tenant."),
        this);
    nextStepsLabel->setWordWrap(true);
    nextStepsLabel->setTextFormat(Qt::RichText);
    nextStepsLayout->addWidget(nextStepsLabel);
    layout->addWidget(nextStepsBox);
}

void TenantApplyAndSummaryPage::initializePage() {
    // Clear the bootstrap flag
    wizard_->clearBootstrapFlag();

    // Build summary
    QString summary = tr("<p>Your tenant has been set up successfully.</p>");

    if (!wizard_->selectedBundleCode().isEmpty()) {
        summary += tr("<p><b>Reference data bundle:</b> %1</p>")
            .arg(wizard_->selectedBundleName());
    }

    if (!wizard_->newAccountUsername().isEmpty()) {
        summary += tr("<p><b>Party admin account created:</b> %1</p>")
            .arg(wizard_->newAccountUsername());
    }

    summary += tr("<p>The bootstrap mode flag has been cleared. "
                  "Please log in with the party admin account to complete "
                  "party setup.</p>");

    summaryLabel_->setText(summary);

    emit wizard_->provisioningCompleted();
}

}
