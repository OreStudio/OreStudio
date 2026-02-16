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
#include "ores.qt/LeiEntityPicker.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGroupBox>
#include <QSizePolicy>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.database/domain/change_reason_constants.hpp"
#include "ores.dq/messaging/publish_bundle_protocol.hpp"
#include "ores.synthetic/messaging/generate_organisation_protocol.hpp"
#include "ores.variability/messaging/feature_flags_protocol.hpp"

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
    setPage(Page_Welcome, new ProvisioningWelcomePage(this));
    setPage(Page_BundleSelection, new BundleSelectionPage(this));
    setPage(Page_BundleInstall, new BundleInstallPage(this));
    setPage(Page_DataSourceSelection, new DataSourceSelectionPage(this));
    setPage(Page_PartySetup, new PartySetupPage(this));
    setPage(Page_CounterpartySetup, new CounterpartySetupPage(this));
    setPage(Page_OrganisationSetup, new OrganisationSetupPage(this));
    setPage(Page_Summary, new ApplyAndSummaryPage(this));

    setStartId(Page_Welcome);
}

void TenantProvisioningWizard::clearBootstrapFlag() {
    BOOST_LOG_SEV(lg(), info) << "Clearing tenant bootstrap mode flag";

    variability::messaging::save_feature_flag_request req;
    req.flag.name = "system.bootstrap_mode";
    req.flag.enabled = false;
    req.flag.description = "Bootstrap mode disabled after tenant setup";
    req.flag.modified_by = clientManager_->currentUsername();
    req.flag.change_reason_code = std::string(reason::codes::new_record);
    req.flag.change_commentary = "Tenant setup wizard completed";

    auto result = clientManager_->process_authenticated_request(std::move(req));
    if (!result) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to clear bootstrap flag: "
                                  << "no response from server";
    } else if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to clear bootstrap flag: "
                                  << result->error_message;
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
           "reference data needed to get started. You can always modify or "
           "extend this data later using the Data Librarian.\n\n"
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
           "<li><b>Choose Data Source</b> - Select between GLEIF registry "
           "or generated synthetic data for parties, counterparties, and "
           "organisational structure.</li>"
           "<li><b>Organisation Setup</b> - Populate your organisation with "
           "the selected data source.</li>"
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
    progressBar_->setStyleSheet("");

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

    appendLog(tr("Publishing catalogue '%1' (mode: upsert, atomic: true)...")
        .arg(wizard_->selectedBundleName()));
}

// ============================================================================
// DataSourceSelectionPage
// ============================================================================

DataSourceSelectionPage::DataSourceSelectionPage(
    TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Choose Data Source"));
    setSubTitle(tr("Select how to populate parties, counterparties, and "
                   "organisational structure for your tenant."));

    setupUI();
}

void DataSourceSelectionPage::setupUI() {
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(12);

    gleifRadio_ = new QRadioButton(
        tr("GLEIF Registry - Search the LEI registry for your organisation"),
        this);
    gleifRadio_->setChecked(true);
    layout->addWidget(gleifRadio_);

    syntheticRadio_ = new QRadioButton(
        tr("Generate Synthetic Data - Create realistic generated data"),
        this);
    layout->addWidget(syntheticRadio_);

    layout->addSpacing(8);

    // Synthetic options group (shown only when synthetic is selected)
    syntheticOptions_ = new QWidget(this);
    auto* optLayout = new QGridLayout(syntheticOptions_);
    optLayout->setContentsMargins(20, 0, 0, 0);

    int row = 0;

    optLayout->addWidget(new QLabel(tr("Country:"), this), row, 0);
    countryCombo_ = new QComboBox(this);
    countryCombo_->addItem(tr("United Kingdom (GB)"), "GB");
    countryCombo_->addItem(tr("United States (US)"), "US");
    optLayout->addWidget(countryCombo_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Party count:"), this), row, 0);
    partyCountSpin_ = new QSpinBox(this);
    partyCountSpin_->setRange(1, 100);
    partyCountSpin_->setValue(5);
    optLayout->addWidget(partyCountSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Counterparty count:"), this), row, 0);
    counterpartyCountSpin_ = new QSpinBox(this);
    counterpartyCountSpin_->setRange(1, 200);
    counterpartyCountSpin_->setValue(10);
    optLayout->addWidget(counterpartyCountSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Portfolio leaf count:"), this), row, 0);
    portfolioLeafCountSpin_ = new QSpinBox(this);
    portfolioLeafCountSpin_->setRange(1, 100);
    portfolioLeafCountSpin_->setValue(8);
    optLayout->addWidget(portfolioLeafCountSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Books per portfolio:"), this), row, 0);
    booksPerPortfolioSpin_ = new QSpinBox(this);
    booksPerPortfolioSpin_->setRange(1, 20);
    booksPerPortfolioSpin_->setValue(2);
    optLayout->addWidget(booksPerPortfolioSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Business unit count:"), this), row, 0);
    businessUnitCountSpin_ = new QSpinBox(this);
    businessUnitCountSpin_->setRange(1, 100);
    businessUnitCountSpin_->setValue(10);
    optLayout->addWidget(businessUnitCountSpin_, row, 1);
    row++;

    generateAddressesCheck_ = new QCheckBox(tr("Generate addresses"), this);
    generateAddressesCheck_->setChecked(true);
    optLayout->addWidget(generateAddressesCheck_, row, 0, 1, 2);
    row++;

    generateIdentifiersCheck_ = new QCheckBox(tr("Generate identifiers (LEI, BIC)"), this);
    generateIdentifiersCheck_->setChecked(true);
    optLayout->addWidget(generateIdentifiersCheck_, row, 0, 1, 2);

    syntheticOptions_->setVisible(false);
    layout->addWidget(syntheticOptions_);

    layout->addStretch();

    connect(gleifRadio_, &QRadioButton::toggled,
            this, &DataSourceSelectionPage::onModeChanged);
    connect(syntheticRadio_, &QRadioButton::toggled,
            this, &DataSourceSelectionPage::onModeChanged);
}

void DataSourceSelectionPage::onModeChanged() {
    syntheticOptions_->setVisible(syntheticRadio_->isChecked());
}

bool DataSourceSelectionPage::validatePage() {
    if (syntheticRadio_->isChecked()) {
        wizard_->setDataSourceMode(
            TenantProvisioningWizard::DataSourceMode::synthetic);
        wizard_->setSyntheticCountry(
            countryCombo_->currentData().toString());
        wizard_->setSyntheticPartyCount(partyCountSpin_->value());
        wizard_->setSyntheticCounterpartyCount(counterpartyCountSpin_->value());
        wizard_->setSyntheticPortfolioLeafCount(portfolioLeafCountSpin_->value());
        wizard_->setSyntheticBooksPerPortfolio(booksPerPortfolioSpin_->value());
        wizard_->setSyntheticBusinessUnitCount(businessUnitCountSpin_->value());
        wizard_->setSyntheticGenerateAddresses(generateAddressesCheck_->isChecked());
        wizard_->setSyntheticGenerateIdentifiers(generateIdentifiersCheck_->isChecked());
    } else {
        wizard_->setDataSourceMode(
            TenantProvisioningWizard::DataSourceMode::gleif);
    }
    return true;
}

int DataSourceSelectionPage::nextId() const {
    if (syntheticRadio_->isChecked()) {
        // Skip LEI/counterparty pages, go straight to organisation setup
        return TenantProvisioningWizard::Page_OrganisationSetup;
    }
    // GLEIF flow: go to party setup
    return TenantProvisioningWizard::Page_PartySetup;
}

// ============================================================================
// PartySetupPage
// ============================================================================

PartySetupPage::PartySetupPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Party Setup (Optional)"));
    setSubTitle(tr("Optionally select a root LEI entity to configure your "
                   "organisation's party hierarchy."));

    setupUI();
}

void PartySetupPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    instructionLabel_ = new QLabel(this);
    instructionLabel_->setWordWrap(true);
    instructionLabel_->setText(
        tr("Search the GLEIF LEI registry for your organisation. The selected "
           "entity will become the root of your party hierarchy.\n\n"
           "This step is optional - you can skip it and set up parties "
           "manually later from the Parties window."));
    layout->addWidget(instructionLabel_);

    layout->addSpacing(10);

    leiPicker_ = new LeiEntityPicker(wizard_->clientManager(), this);
    layout->addWidget(leiPicker_);
}

void PartySetupPage::initializePage() {
    if (!leiLoaded_) {
        leiPicker_->load();
        leiLoaded_ = true;
    }
}

bool PartySetupPage::validatePage() {
    // Optional page - always allow advancing
    if (leiPicker_->hasSelection()) {
        wizard_->setRootLei(leiPicker_->selectedLei());
        wizard_->setRootLeiName(leiPicker_->selectedName());
    }
    return true;
}

// ============================================================================
// CounterpartySetupPage
// ============================================================================

CounterpartySetupPage::CounterpartySetupPage(
    TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Counterparty Import"));
    setupUI();
}

void CounterpartySetupPage::setupUI() {
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* infoLabel = new QLabel(this);
    infoLabel->setWordWrap(true);
    infoLabel->setText(
        tr("Counterparties represent the external entities your organisation "
           "trades with or has business relationships with.\n\n"
           "Counterparty import is a planned feature that will allow you to "
           "bulk-import counterparties from external sources such as:\n\n"
           "  - GLEIF LEI registry\n"
           "  - CSV/Excel files\n"
           "  - External APIs\n\n"
           "For now, you can add counterparties manually from the "
           "Counterparties window after completing this wizard."));
    layout->addWidget(infoLabel);

    layout->addStretch();

    auto* noteBox = new QGroupBox(tr("Coming Soon"), this);
    auto* noteLayout = new QVBoxLayout(noteBox);
    auto* noteLabel = new QLabel(
        tr("Automated counterparty import will be available in a future "
           "release. Click Next to continue."),
        this);
    noteLabel->setWordWrap(true);
    noteLayout->addWidget(noteLabel);
    layout->addWidget(noteBox);
}

// ============================================================================
// OrganisationSetupPage
// ============================================================================

OrganisationSetupPage::OrganisationSetupPage(
    TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Organisation Setup"));
    setSubTitle(tr("Publishing sample business units, portfolios, and trading "
                   "books for your organisation."));
    setFinalPage(false);

    auto* layout = new QVBoxLayout(this);

    statusLabel_ = new QLabel(tr("Starting..."), this);
    statusLabel_->setStyleSheet("font-weight: bold;");
    layout->addWidget(statusLabel_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0); // indeterminate
    layout->addWidget(progressBar_);

    logOutput_ = new QTextEdit(this);
    logOutput_->setReadOnly(true);
    logOutput_->setFont(FontUtils::monospace());
    layout->addWidget(logOutput_);
}

bool OrganisationSetupPage::isComplete() const {
    return publishComplete_;
}

void OrganisationSetupPage::initializePage() {
    publishComplete_ = false;
    publishSuccess_ = false;
    logOutput_->clear();
    progressBar_->setRange(0, 0);
    progressBar_->setStyleSheet("");

    const bool isSynthetic = wizard_->dataSourceMode() ==
        TenantProvisioningWizard::DataSourceMode::synthetic;

    if (isSynthetic) {
        setSubTitle(tr("Generating synthetic parties, counterparties, "
                       "business units, portfolios, and trading books."));
        statusLabel_->setText(tr("Generating synthetic organisation data..."));
    } else {
        setSubTitle(tr("Publishing sample business units, portfolios, and "
                       "trading books for your organisation."));
        statusLabel_->setText(tr("Publishing organisation data..."));
    }

    startPublish();
}

void OrganisationSetupPage::appendLog(const QString& message) {
    logOutput_->append(message);
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void OrganisationSetupPage::startPublish() {
    const std::string publishedBy = wizard_->clientManager()->currentUsername();
    ClientManager* clientManager = wizard_->clientManager();

    const bool isSynthetic = wizard_->dataSourceMode() ==
        TenantProvisioningWizard::DataSourceMode::synthetic;

    if (isSynthetic) {
        startSyntheticGeneration();
    } else {
        startBundlePublish();
    }
}

void OrganisationSetupPage::startBundlePublish() {
    const std::string publishedBy = wizard_->clientManager()->currentUsername();
    ClientManager* clientManager = wizard_->clientManager();

    BOOST_LOG_SEV(lg(), info) << "Publishing organisation bundle";

    using ResponseType = dq::messaging::publish_bundle_response;

    auto* watcher = new QFutureWatcher<std::optional<ResponseType>>(this);
    connect(watcher, &QFutureWatcher<std::optional<ResponseType>>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        if (!result || !result->success) {
            publishSuccess_ = false;
            statusLabel_->setText(tr("Publication failed!"));
            progressBar_->setStyleSheet(
                "QProgressBar::chunk { background-color: #cc0000; }");

            if (!result) {
                BOOST_LOG_SEV(lg(), error)
                    << "Organisation publication: no server response";
                appendLog(tr("ERROR: Failed to communicate with server."));
            } else {
                BOOST_LOG_SEV(lg(), error)
                    << "Organisation publication failed: "
                    << result->error_message;
                appendLog(tr("ERROR: %1").arg(
                    QString::fromStdString(result->error_message)));
            }
        } else {
            BOOST_LOG_SEV(lg(), info)
                << "Organisation publication succeeded: "
                << result->datasets_succeeded << " datasets";
            statusLabel_->setText(
                tr("Organisation data published successfully!"));
            appendLog(tr("Published %1 datasets (%2 records inserted, %3 updated).")
                .arg(result->datasets_succeeded)
                .arg(result->total_records_inserted)
                .arg(result->total_records_updated));
            publishSuccess_ = true;
            wizard_->setOrganisationPublished(true);
        }

        publishComplete_ = true;
        emit completeChanged();
    });

    QFuture<std::optional<ResponseType>> future = QtConcurrent::run(
        [clientManager, publishedBy]() -> std::optional<ResponseType> {

            dq::messaging::publish_bundle_request request;
            request.bundle_code = "organisation";
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

    appendLog(tr("Publishing organisation bundle (business units, portfolios, "
                  "books)..."));
}

void OrganisationSetupPage::startSyntheticGeneration() {
    ClientManager* clientManager = wizard_->clientManager();

    BOOST_LOG_SEV(lg(), info) << "Generating synthetic organisation data";

    using ResponseType = synthetic::messaging::generate_organisation_response;

    // Build request struct upfront to avoid a long capture list.
    synthetic::messaging::generate_organisation_request request;
    request.country = wizard_->syntheticCountry().toStdString();
    request.party_count =
        static_cast<std::uint32_t>(wizard_->syntheticPartyCount());
    request.counterparty_count =
        static_cast<std::uint32_t>(wizard_->syntheticCounterpartyCount());
    request.portfolio_leaf_count =
        static_cast<std::uint32_t>(wizard_->syntheticPortfolioLeafCount());
    request.books_per_leaf_portfolio =
        static_cast<std::uint32_t>(wizard_->syntheticBooksPerPortfolio());
    request.business_unit_count =
        static_cast<std::uint32_t>(wizard_->syntheticBusinessUnitCount());
    request.generate_addresses = wizard_->syntheticGenerateAddresses();
    request.generate_identifiers = wizard_->syntheticGenerateIdentifiers();
    request.published_by = clientManager->currentUsername();

    auto* watcher = new QFutureWatcher<std::optional<ResponseType>>(this);
    connect(watcher, &QFutureWatcher<std::optional<ResponseType>>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        if (!result || !result->success) {
            publishSuccess_ = false;
            statusLabel_->setText(tr("Generation failed!"));
            progressBar_->setStyleSheet(
                "QProgressBar::chunk { background-color: #cc0000; }");

            if (!result) {
                BOOST_LOG_SEV(lg(), error)
                    << "Synthetic generation: no server response";
                appendLog(tr("ERROR: Failed to communicate with server."));
            } else {
                BOOST_LOG_SEV(lg(), error)
                    << "Synthetic generation failed: "
                    << result->error_message;
                appendLog(tr("ERROR: %1").arg(
                    QString::fromStdString(result->error_message)));
            }
        } else {
            BOOST_LOG_SEV(lg(), info)
                << "Synthetic generation succeeded: "
                << result->parties_count << " parties, "
                << result->counterparties_count << " counterparties";
            statusLabel_->setText(
                tr("Synthetic organisation generated successfully!"));
            appendLog(tr("Generated %1 parties, %2 counterparties, "
                         "%3 portfolios, %4 books, %5 business units, "
                         "%6 contacts, %7 identifiers.")
                .arg(result->parties_count)
                .arg(result->counterparties_count)
                .arg(result->portfolios_count)
                .arg(result->books_count)
                .arg(result->business_units_count)
                .arg(result->contacts_count)
                .arg(result->identifiers_count));
            publishSuccess_ = true;
            wizard_->setOrganisationPublished(true);
        }

        publishComplete_ = true;
        emit completeChanged();
    });

    QFuture<std::optional<ResponseType>> future = QtConcurrent::run(
        [clientManager, request = std::move(request)]() mutable
            -> std::optional<ResponseType> {

            auto result = clientManager->process_authenticated_request(
                std::move(request));

            if (!result) {
                return std::nullopt;
            }
            return *result;
        }
    );

    watcher->setFuture(future);

    appendLog(tr("Generating synthetic organisation (country: %1, "
                  "parties: %2, counterparties: %3)...")
        .arg(wizard_->syntheticCountry())
        .arg(wizard_->syntheticPartyCount())
        .arg(wizard_->syntheticCounterpartyCount()));
}

// ============================================================================
// ApplyAndSummaryPage
// ============================================================================

ApplyAndSummaryPage::ApplyAndSummaryPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Setup Complete"));
    setFinalPage(true);

    setupUI();
}

void ApplyAndSummaryPage::setupUI() {
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
        tr("You can now use the full application. Some things you might "
           "want to do:\n\n"
           "  - Open the <b>Data Librarian</b> to manage datasets and bundles\n"
           "  - Open <b>Parties</b> to manage your party hierarchy\n"
           "  - Open <b>Counterparties</b> to add trading counterparties\n"
           "  - Open <b>Accounts</b> to create additional user accounts"),
        this);
    nextStepsLabel->setWordWrap(true);
    nextStepsLabel->setTextFormat(Qt::RichText);
    nextStepsLayout->addWidget(nextStepsLabel);
    layout->addWidget(nextStepsBox);
}

void ApplyAndSummaryPage::initializePage() {
    // Clear the bootstrap flag
    wizard_->clearBootstrapFlag();

    // Build summary
    QString summary = tr("<p>Your tenant has been set up successfully.</p>");

    if (!wizard_->selectedBundleCode().isEmpty()) {
        summary += tr("<p><b>Reference data bundle:</b> %1</p>")
            .arg(wizard_->selectedBundleName());
    }

    if (!wizard_->rootLei().isEmpty()) {
        summary += tr("<p><b>Root party (LEI):</b> %1 (%2)</p>")
            .arg(wizard_->rootLeiName(), wizard_->rootLei());
    }

    if (wizard_->organisationPublished()) {
        if (wizard_->dataSourceMode() ==
            TenantProvisioningWizard::DataSourceMode::synthetic) {
            summary += tr("<p><b>Organisation data:</b> Synthetic parties, "
                          "counterparties, business units, portfolios, and "
                          "trading books generated.</p>");
        } else {
            summary += tr("<p><b>Organisation data:</b> Business units, portfolios, "
                          "and trading books published.</p>");
        }
    }

    summary += tr("<p>The bootstrap mode flag has been cleared. This wizard "
                  "will not appear on your next login.</p>");

    summaryLabel_->setText(summary);

    emit wizard_->provisioningCompleted();
}

}
