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
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.qt.headless/FontUtils.hpp"
#include "ores.qt/ClientDatasetBundleModel.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LeiEntityPicker.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.synthetic.api/messaging/generate_organisation_protocol.hpp"
#include <QFormLayout>
#include <QFutureWatcher>
#include <QGridLayout>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QListWidgetItem>
#include <QRegularExpressionValidator>
#include <QSizePolicy>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <chrono>

namespace ores::qt {

using namespace ores::logging;
namespace reason = ores::dq::domain::change_reason_constants;

// ============================================================================
// TenantProvisioningWizard
// ============================================================================

TenantProvisioningWizard::TenantProvisioningWizard(ClientManager* clientManager, QWidget* parent)
    : QWizard(parent)
    , clientManager_(clientManager) {

    setWindowTitle(tr("New Tenant Provisioner"));
    setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::BuildingSkyscraper, IconUtils::DefaultIconColor));
    setMinimumSize(900, 700);
    resize(900, 700);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage, true);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    setupPages();

    // Clear bootstrap flag on cancel/reject too
    connect(this, &QWizard::rejected, this, [this]() { clearBootstrapFlag(); });
}

void TenantProvisioningWizard::setupPages() {
    WidgetUtils::setupComboBoxes(this);
    setPage(Page_Welcome, new ProvisioningWelcomePage(this));
    setPage(Page_BundleSelection, new BundleSelectionPage(this));
    setPage(Page_DataSourceSelection, new TenantDataSourceSelectionPage(this));
    setPage(Page_PartySetup, new TenantPartySetupPage(this));
    setPage(Page_Execute, new TenantExecutePage(this));
    setPage(Page_Summary, new TenantApplyAndSummaryPage(this));

    setStartId(Page_Welcome);
}

void TenantProvisioningWizard::clearBootstrapFlag() {
    // complete_tenant_provisioning_command clears system.bootstrap_mode (and
    // sets onboarding.tenant = true) server-side over its own NATS-routed,
    // permission-check-free path — see
    // ores.iam.core/messaging/tenant_handler.hpp. No separate client-side
    // save_setting_request is needed (a prior version issued one
    // pre-emptively here, but it was redundant with the authoritative
    // server-side clear and risked writing a party-scoped duplicate row if
    // the acting user had a party selected).
    BOOST_LOG_SEV(lg(), info) << "Marking tenant active";
    iam::messaging::complete_tenant_provisioning_command activateReq;
    auto activateResult = clientManager_->process_authenticated_request(std::move(activateReq));
    if (!activateResult) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to mark tenant active: no response from server";
    } else if (!activateResult->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to mark tenant active: "
                                  << (activateResult->message.empty() ? "Unknown error" :
                                                                        activateResult->message);
    } else {
        BOOST_LOG_SEV(lg(), info) << "Tenant marked active successfully";
    }
}

// ============================================================================
// ProvisioningWelcomePage
// ============================================================================

ProvisioningWelcomePage::ProvisioningWelcomePage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Welcome"));
    setupUI();
}

void ProvisioningWelcomePage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* welcomeLabel = new QLabel(tr("Your tenant is new and needs initial setup."), this);
    welcomeLabel->setStyleSheet("font-size: 16pt; font-weight: bold;");
    welcomeLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(welcomeLabel);

    layout->addSpacing(10);

    auto* descLabel = new QLabel(this);
    descLabel->setWordWrap(true);
    descLabel->setText(tr("This wizard will help you set up your tenant with essential "
                          "reference data, parties, and organisational structure.\n\n"
                          "The setup process includes:"));
    layout->addWidget(descLabel);

    auto* stepsLabel = new QLabel(this);
    stepsLabel->setWordWrap(true);
    stepsLabel->setTextFormat(Qt::RichText);
    stepsLabel->setText(tr("<ol>"
                           "<li><b>Select Catalogue</b> - Choose a pre-configured set of "
                           "reference data (currencies, countries, etc.).</li>"
                           "<li><b>Party Structure</b> - Choose how to set up your organisation's "
                           "party hierarchy (GLEIF registry or generated data).</li>"
                           "<li><b>Execute</b> - All data is published and configured in one step "
                           "with live progress tracking.</li>"
                           "</ol>"));
    layout->addWidget(stepsLabel);

    layout->addStretch();

    auto* noteBox = new QGroupBox(tr("Note"), this);
    auto* noteLayout = new QVBoxLayout(noteBox);
    auto* noteLabel =
        new QLabel(tr("You can skip this setup by clicking Cancel. The wizard will not "
                      "appear again, and you can set up reference data and parties "
                      "manually using the Data Librarian and Parties window."),
                   this);
    noteLabel->setWordWrap(true);
    noteLayout->addWidget(noteLabel);
    layout->addWidget(noteBox);
}

// ============================================================================
// BundleSelectionPage
// ============================================================================

BundleSelectionPage::BundleSelectionPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Select Catalogue"));
    setSubTitle(tr("Choose a catalogue of reference data to publish to your "
                   "tenant. Each catalogue contains a pre-configured set of "
                   "currencies, countries, and other reference data."));

    setupUI();
}

void BundleSelectionPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    bundleModel_ = new ClientDatasetBundleModel(wizard_->clientManager(), this);

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

    connect(bundleCombo_,
            QOverload<int>::of(&QComboBox::currentIndexChanged),
            this,
            &BundleSelectionPage::onBundleChanged);

    // Populate combo when data arrives
    connect(bundleModel_, &ClientDatasetBundleModel::dataLoaded, this, [this]() {
        bundleCombo_->clear();
        const int count = bundleModel_->rowCount();
        for (int i = 0; i < count; ++i) {
            const auto* bundle = bundleModel_->getBundle(i);
            if (bundle) {
                bundleCombo_->addItem(QString::fromStdString(bundle->name), i);
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

    connect(bundleModel_, &ClientDatasetBundleModel::loadError, this, [this](const QString& msg) {
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
        descriptionLabel_->setText(QString::fromStdString(bundle->description));
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
// TenantDataSourceSelectionPage
// ============================================================================

TenantDataSourceSelectionPage::TenantDataSourceSelectionPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Choose Data Source"));
    setSubTitle(tr("Select how to populate the party hierarchy for your tenant."));

    setupUI();
}

void TenantDataSourceSelectionPage::setupUI() {
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(12);

    gleifRadio_ = new QRadioButton(
        tr("GLEIF Registry - Search the LEI registry for your organisation"), this);
    gleifRadio_->setChecked(true);
    layout->addWidget(gleifRadio_);

    syntheticRadio_ =
        new QRadioButton(tr("Generate Synthetic Data - Create realistic generated data"), this);
    layout->addWidget(syntheticRadio_);

    layout->addSpacing(8);

    // Synthetic options group (shown only when synthetic is selected)
    syntheticOptions_ = new QWidget(this);
    auto* optLayout = new QGridLayout(syntheticOptions_);
    optLayout->setContentsMargins(20, 0, 0, 0);
    optLayout->setColumnStretch(1, 1);

    int row = 0;

    // --- Entity counts -------------------------------------------------------
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

    // --- Hierarchy depths ----------------------------------------------------
    auto* depthSep = new QLabel(tr("<b>Hierarchy depths</b>"), this);
    depthSep->setContentsMargins(0, 6, 0, 2);
    optLayout->addWidget(depthSep, row, 0, 1, 2);
    row++;

    optLayout->addWidget(new QLabel(tr("Party max depth:"), this), row, 0);
    partyMaxDepthSpin_ = new QSpinBox(this);
    partyMaxDepthSpin_->setRange(1, 10);
    partyMaxDepthSpin_->setValue(3);
    optLayout->addWidget(partyMaxDepthSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Counterparty max depth:"), this), row, 0);
    counterpartyMaxDepthSpin_ = new QSpinBox(this);
    counterpartyMaxDepthSpin_->setRange(1, 10);
    counterpartyMaxDepthSpin_->setValue(3);
    optLayout->addWidget(counterpartyMaxDepthSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Portfolio max depth:"), this), row, 0);
    portfolioMaxDepthSpin_ = new QSpinBox(this);
    portfolioMaxDepthSpin_->setRange(1, 10);
    portfolioMaxDepthSpin_->setValue(4);
    optLayout->addWidget(portfolioMaxDepthSpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Business unit max depth:"), this), row, 0);
    businessUnitMaxDepthSpin_ = new QSpinBox(this);
    businessUnitMaxDepthSpin_->setRange(1, 5);
    businessUnitMaxDepthSpin_->setValue(2);
    optLayout->addWidget(businessUnitMaxDepthSpin_, row, 1);
    row++;

    // --- Contact and identifier options --------------------------------------
    auto* contactSep = new QLabel(tr("<b>Contacts &amp; identifiers</b>"), this);
    contactSep->setContentsMargins(0, 6, 0, 2);
    optLayout->addWidget(contactSep, row, 0, 1, 2);
    row++;

    optLayout->addWidget(new QLabel(tr("Contacts per party:"), this), row, 0);
    contactsPerPartySpin_ = new QSpinBox(this);
    contactsPerPartySpin_->setRange(0, 10);
    contactsPerPartySpin_->setValue(2);
    optLayout->addWidget(contactsPerPartySpin_, row, 1);
    row++;

    optLayout->addWidget(new QLabel(tr("Contacts per counterparty:"), this), row, 0);
    contactsPerCounterpartySpin_ = new QSpinBox(this);
    contactsPerCounterpartySpin_->setRange(0, 10);
    contactsPerCounterpartySpin_->setValue(1);
    optLayout->addWidget(contactsPerCounterpartySpin_, row, 1);
    row++;

    generateAddressesCheck_ = new QCheckBox(tr("Generate addresses"), this);
    generateAddressesCheck_->setChecked(true);
    optLayout->addWidget(generateAddressesCheck_, row, 0, 1, 2);
    row++;

    generateIdentifiersCheck_ = new QCheckBox(tr("Generate identifiers (LEI, BIC)"), this);
    generateIdentifiersCheck_->setChecked(true);
    optLayout->addWidget(generateIdentifiersCheck_, row, 0, 1, 2);
    row++;

    // --- Reproducibility -----------------------------------------------------
    auto* seedSep = new QLabel(tr("<b>Reproducibility</b>"), this);
    seedSep->setContentsMargins(0, 6, 0, 2);
    optLayout->addWidget(seedSep, row, 0, 1, 2);
    row++;

    optLayout->addWidget(new QLabel(tr("Seed (optional):"), this), row, 0);
    seedEdit_ = new QLineEdit(this);
    seedEdit_->setPlaceholderText(tr("Leave blank for a random seed"));
    seedEdit_->setValidator(
        new QRegularExpressionValidator(QRegularExpression("[0-9]*"), seedEdit_));
    optLayout->addWidget(seedEdit_, row, 1);

    syntheticOptions_->setVisible(false);
    layout->addWidget(syntheticOptions_);

    layout->addStretch();

    connect(
        gleifRadio_, &QRadioButton::toggled, this, &TenantDataSourceSelectionPage::onModeChanged);
    connect(syntheticRadio_,
            &QRadioButton::toggled,
            this,
            &TenantDataSourceSelectionPage::onModeChanged);

    WidgetUtils::setupComboBoxes(this);
}

void TenantDataSourceSelectionPage::onModeChanged() {
    syntheticOptions_->setVisible(syntheticRadio_->isChecked());
}

bool TenantDataSourceSelectionPage::validatePage() {
    if (syntheticRadio_->isChecked()) {
        wizard_->setDataSourceMode(TenantProvisioningWizard::DataSourceMode::synthetic);
        wizard_->setSyntheticCountry(countryCombo_->currentData().toString());
        wizard_->setSyntheticPartyCount(partyCountSpin_->value());
        wizard_->setSyntheticPartyMaxDepth(partyMaxDepthSpin_->value());
        wizard_->setSyntheticCounterpartyCount(counterpartyCountSpin_->value());
        wizard_->setSyntheticCounterpartyMaxDepth(counterpartyMaxDepthSpin_->value());
        wizard_->setSyntheticPortfolioLeafCount(portfolioLeafCountSpin_->value());
        wizard_->setSyntheticPortfolioMaxDepth(portfolioMaxDepthSpin_->value());
        wizard_->setSyntheticBooksPerPortfolio(booksPerPortfolioSpin_->value());
        wizard_->setSyntheticBusinessUnitCount(businessUnitCountSpin_->value());
        wizard_->setSyntheticBusinessUnitMaxDepth(businessUnitMaxDepthSpin_->value());
        wizard_->setSyntheticGenerateAddresses(generateAddressesCheck_->isChecked());
        wizard_->setSyntheticContactsPerParty(contactsPerPartySpin_->value());
        wizard_->setSyntheticContactsPerCounterparty(contactsPerCounterpartySpin_->value());
        wizard_->setSyntheticGenerateIdentifiers(generateIdentifiersCheck_->isChecked());

        const auto seedText = seedEdit_->text().trimmed();
        if (seedText.isEmpty()) {
            wizard_->setSyntheticSeed(std::nullopt);
        } else {
            bool ok = false;
            const auto seed = seedText.toULongLong(&ok);
            wizard_->setSyntheticSeed(ok ? std::optional<std::uint64_t>{seed} : std::nullopt);
        }
    } else {
        wizard_->setDataSourceMode(TenantProvisioningWizard::DataSourceMode::gleif);
    }
    return true;
}

int TenantDataSourceSelectionPage::nextId() const {
    // Synthetic mode skips the LEI picker page (PartySetup) and goes straight to Execute.
    if (syntheticRadio_->isChecked())
        return TenantProvisioningWizard::Page_Execute;
    return TenantProvisioningWizard::Page_PartySetup;
}

// ============================================================================
// TenantPartySetupPage
// ============================================================================

TenantPartySetupPage::TenantPartySetupPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Party Setup (Optional)"));
    setSubTitle(tr("Optionally select a root LEI entity to configure your "
                   "organisation's party hierarchy."));

    setupUI();
}

void TenantPartySetupPage::setupUI() {
    auto* layout = new QVBoxLayout(this);

    instructionLabel_ = new QLabel(this);
    instructionLabel_->setWordWrap(true);
    instructionLabel_->setText(
        tr("Search the GLEIF LEI registry for your organisation. The selected "
           "entity will become the root of your party hierarchy. All child "
           "entities in the GLEIF registry will be created automatically.\n\n"
           "This step is optional - you can skip it and set up parties "
           "manually later from the Parties window."));
    layout->addWidget(instructionLabel_);

    layout->addSpacing(10);

    // Dataset size selection
    auto* sizeLayout = new QHBoxLayout();
    sizeLayout->addWidget(new QLabel(tr("LEI dataset size:"), this));
    datasetSizeCombo_ = new QComboBox(this);
    datasetSizeCombo_->addItem(tr("Large (~15,000 entities)"), "large");
    datasetSizeCombo_->addItem(tr("Small (~6,000 entities)"), "small");
    datasetSizeCombo_->setCurrentIndex(0);
    sizeLayout->addWidget(datasetSizeCombo_);
    sizeLayout->addStretch();
    layout->addLayout(sizeLayout);

    layout->addSpacing(10);

    leiPicker_ = new LeiEntityPicker(wizard_->clientManager(), this);
    layout->addWidget(leiPicker_);

    WidgetUtils::setupComboBoxes(this);
}

void TenantPartySetupPage::initializePage() {
    if (!leiLoaded_) {
        leiPicker_->load();
        leiLoaded_ = true;
    }
}

bool TenantPartySetupPage::validatePage() {
    // Optional page - always allow advancing
    if (leiPicker_->hasSelection()) {
        wizard_->setRootLei(leiPicker_->selectedLei());
        wizard_->setRootLeiName(leiPicker_->selectedName());
        BOOST_LOG_SEV(lg(), info) << "Party setup: root LEI set to "
                                  << leiPicker_->selectedLei().toStdString() << " ("
                                  << leiPicker_->selectedName().toStdString() << ")";
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Party setup: no LEI selected, advancing without root LEI";
    }
    wizard_->setLeiDatasetSize(datasetSizeCombo_->currentData().toString());
    return true;
}

// ============================================================================
// TenantExecutePage
// ============================================================================

TenantExecutePage::TenantExecutePage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Setting Up Tenant"));
    setSubTitle(tr("Publishing reference data and configuring your organisation. "
                   "Please wait — this may take several minutes."));
    setCommitPage(true); // Disable Back once execution starts to prevent re-running.
    setFinalPage(false);

    auto* layout = new QVBoxLayout(this);

    statusLabel_ = new QLabel(tr("Starting..."), this);
    statusLabel_->setStyleSheet("font-weight: bold;");
    layout->addWidget(statusLabel_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0);
    progressBar_->setTextVisible(false);
    progressBar_->setStyleSheet("QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
                                "background: #2d2d2d; height: 20px; }"
                                "QProgressBar::chunk { background-color: #4a9eff; }");
    layout->addWidget(progressBar_);

    stepsWidget_ = new WorkflowStepsWidget(wizard_->clientManager(), this);
    stepsWidget_->setVisible(false);
    stepsWidget_->setMinimumHeight(200);
    stepsWidget_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    layout->addWidget(stepsWidget_);

    logOutput_ = new QTextEdit(this);
    logOutput_->setReadOnly(true);
    logOutput_->setFont(FontUtils::monospace());
    logOutput_->setMaximumHeight(120);
    layout->addWidget(logOutput_);

    connect(stepsWidget_,
            &WorkflowStepsWidget::instanceReachedTerminalState,
            this,
            &TenantExecutePage::onWorkflowComplete);
}

bool TenantExecutePage::isComplete() const {
    return allComplete_;
}

void TenantExecutePage::initializePage() {
    allComplete_ = false;
    allSuccess_ = false;
    partiesLinked_ = 0;
    publishedBy_ = wizard_->clientManager()->currentUsername();

    logOutput_->clear();
    stepsWidget_->setVisible(false);
    progressBar_->setRange(0, 0);
    progressBar_->setVisible(true);
    progressBar_->setStyleSheet("QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
                                "background: #2d2d2d; height: 20px; }"
                                "QProgressBar::chunk { background-color: #4a9eff; }");
    statusLabel_->setStyleSheet("font-weight: bold;");
    statusLabel_->setText(
        tr("Publishing base catalogue '%1'...").arg(wizard_->selectedBundleName()));

    startBundlePublish();
}

void TenantExecutePage::appendLog(const QString& msg) {
    logOutput_->append(msg);
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void TenantExecutePage::markFailed(const QString& errorMsg) {
    BOOST_LOG_SEV(lg(), error) << "Tenant setup failed: " << errorMsg.toStdString();
    statusLabel_->setText(tr("Setup failed: %1").arg(errorMsg));
    statusLabel_->setStyleSheet("font-weight: bold; color: #cc0000;");
    progressBar_->setRange(0, 1);
    progressBar_->setValue(1);
    progressBar_->setStyleSheet("QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
                                "background: #2d2d2d; height: 20px; }"
                                "QProgressBar::chunk { background-color: #cc0000; }");
    appendLog(tr("ERROR: %1").arg(errorMsg));
    allSuccess_ = false;
    allComplete_ = true;
    emit completeChanged();
}

// Phase 1: Publish base bundle (with LEI params if GLEIF mode + root LEI selected).
void TenantExecutePage::startBundlePublish() {
    const std::string bundleCode = wizard_->selectedBundleCode().toStdString();
    ClientManager* clientManager = wizard_->clientManager();

    // Build params: include LEI data only when we have a root LEI to publish.
    std::string paramsJson;
    const bool isGleif =
        wizard_->dataSourceMode() == TenantProvisioningWizard::DataSourceMode::gleif;
    const std::string rootLei = wizard_->rootLei().toStdString();
    const bool hasLei = isGleif && !rootLei.empty();

    if (hasLei) {
        dq::messaging::publish_bundle_params params;
        params.lei_parties = dq::messaging::lei_parties_params{rootLei};
        paramsJson = dq::messaging::build_params_json(params);
        appendLog(tr("Publishing catalogue '%1' with GLEIF root LEI: %2 (dataset: %3)")
                      .arg(wizard_->selectedBundleName())
                      .arg(wizard_->rootLeiName())
                      .arg(wizard_->leiDatasetSize()));
        BOOST_LOG_SEV(lg(), info) << "Phase 1: publishing bundle=" << bundleCode
                                  << " root_lei=" << rootLei
                                  << " size=" << wizard_->leiDatasetSize().toStdString();
    } else {
        appendLog(tr("Publishing catalogue '%1'...").arg(wizard_->selectedBundleName()));
        BOOST_LOG_SEV(lg(), info) << "Phase 1: publishing bundle=" << bundleCode
                                  << " (no LEI params)";
    }

    struct BundleResult {
        bool success = false;
        std::string error_message;
        std::string instance_id;
        int datasets_dispatched = 0;
    };

    auto* watcher = new QFutureWatcher<BundleResult>(this);
    connect(watcher, &QFutureWatcher<BundleResult>::finished, this, [this, watcher]() {
        BundleResult result;
        try {
            result = watcher->result();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Phase 1 async task threw: " << e.what();
            result.error_message = e.what();
        }
        watcher->deleteLater();

        if (!result.success) {
            markFailed(QString::fromStdString(result.error_message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Bundle workflow started: instance=" << result.instance_id
                                  << " datasets=" << result.datasets_dispatched;
        appendLog(tr("Catalogue workflow started: %1 datasets dispatched.")
                      .arg(result.datasets_dispatched));
        statusLabel_->setText(
            tr("Publishing datasets... (%1 dispatched)").arg(result.datasets_dispatched));

        progressBar_->setVisible(false);
        stepsWidget_->setVisible(true);
        stepsWidget_->setInstance(QUuid::fromString(QString::fromStdString(result.instance_id)));
        stepsWidget_->preSeed(result.datasets_dispatched);
        // onWorkflowComplete fires via instanceReachedTerminalState
    });

    QFuture<BundleResult> future = QtConcurrent::run(
        [clientManager, bundleCode, paramsJson, publishedBy = publishedBy_]() -> BundleResult {
            BundleResult result;
            dq::messaging::publish_bundle_request request;
            request.bundle_code = bundleCode;
            request.mode = dq::domain::publication_mode::upsert;
            request.published_by = publishedBy;
            request.atomic = true;
            request.params_json = paramsJson;

            auto resp = clientManager->process_authenticated_request(std::move(request),
                                                                     std::chrono::minutes(5));

            if (!resp) {
                result.error_message = "Failed to communicate with server (bundle publish)";
                return result;
            }
            if (!resp->success) {
                result.error_message = resp->error_message;
                return result;
            }
            result.success = true;
            result.instance_id = resp->instance_id;
            result.datasets_dispatched = resp->datasets_dispatched;
            return result;
        });

    watcher->setFuture(future);
}

// Called when the base bundle workflow reaches a terminal state.
void TenantExecutePage::onWorkflowComplete(bool success) {
    // Leave stepsWidget_ visible — completed table stays as a record.
    // Show progressBar_ as a spinner for subsequent non-workflow phases.
    progressBar_->setVisible(true);
    progressBar_->setRange(0, 0);

    if (!success) {
        markFailed(tr("Base catalogue workflow completed with errors."));
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Phase 1 complete: base bundle workflow succeeded";
    appendLog(tr("Reference data published successfully."));

    const bool isSynthetic =
        wizard_->dataSourceMode() == TenantProvisioningWizard::DataSourceMode::synthetic;

    if (isSynthetic) {
        statusLabel_->setText(tr("Generating synthetic organisation data..."));
        startSyntheticGeneration();
    } else {
        statusLabel_->setText(tr("Associating tenant admin with parties..."));
        startPartyAssociation();
    }
}

// Phase 2 (synthetic mode only): generate synthetic organisation data.
void TenantExecutePage::startSyntheticGeneration() {
    ClientManager* clientManager = wizard_->clientManager();

    BOOST_LOG_SEV(lg(), info) << "Phase 2: generating synthetic organisation";
    appendLog(tr("Generating synthetic organisation data..."));

    struct SyntheticResult {
        bool success = false;
        std::string error_message;
        int parties_generated = 0;
    };

    auto* watcher = new QFutureWatcher<SyntheticResult>(this);
    connect(watcher, &QFutureWatcher<SyntheticResult>::finished, this, [this, watcher]() {
        SyntheticResult result;
        try {
            result = watcher->result();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Phase 2 async task threw: " << e.what();
            result.error_message = e.what();
        }
        watcher->deleteLater();

        if (!result.success) {
            markFailed(QString::fromStdString(result.error_message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Synthetic generation complete: " << result.parties_generated
                                  << " parties";
        appendLog(
            tr("Synthetic data generated: %1 parties created.").arg(result.parties_generated));
        statusLabel_->setText(tr("Associating tenant admin with parties..."));
        startPartyAssociation();
    });

    QFuture<SyntheticResult> future =
        QtConcurrent::run([clientManager,
                           country = wizard_->syntheticCountry().toStdString(),
                           partyCount = wizard_->syntheticPartyCount(),
                           partyMaxDepth = wizard_->syntheticPartyMaxDepth(),
                           counterpartyCount = wizard_->syntheticCounterpartyCount(),
                           counterpartyMaxDepth = wizard_->syntheticCounterpartyMaxDepth(),
                           portfolioLeafCount = wizard_->syntheticPortfolioLeafCount(),
                           portfolioMaxDepth = wizard_->syntheticPortfolioMaxDepth(),
                           booksPerPortfolio = wizard_->syntheticBooksPerPortfolio(),
                           businessUnitCount = wizard_->syntheticBusinessUnitCount(),
                           businessUnitMaxDepth = wizard_->syntheticBusinessUnitMaxDepth(),
                           generateAddresses = wizard_->syntheticGenerateAddresses(),
                           contactsPerParty = wizard_->syntheticContactsPerParty(),
                           contactsPerCp = wizard_->syntheticContactsPerCounterparty(),
                           generateIdentifiers = wizard_->syntheticGenerateIdentifiers(),
                           seed = wizard_->syntheticSeed()]() -> SyntheticResult {
            SyntheticResult result;
            synthetic::messaging::generate_organisation_request request;
            request.country = country;
            request.party_count = static_cast<std::uint32_t>(partyCount);
            request.party_max_depth = static_cast<std::uint32_t>(partyMaxDepth);
            request.counterparty_count = static_cast<std::uint32_t>(counterpartyCount);
            request.counterparty_max_depth = static_cast<std::uint32_t>(counterpartyMaxDepth);
            request.portfolio_leaf_count = static_cast<std::uint32_t>(portfolioLeafCount);
            request.portfolio_max_depth = static_cast<std::uint32_t>(portfolioMaxDepth);
            request.books_per_leaf_portfolio = static_cast<std::uint32_t>(booksPerPortfolio);
            request.business_unit_count = static_cast<std::uint32_t>(businessUnitCount);
            request.business_unit_max_depth = static_cast<std::uint32_t>(businessUnitMaxDepth);
            request.generate_addresses = generateAddresses;
            request.contacts_per_party = static_cast<std::uint32_t>(contactsPerParty);
            request.contacts_per_counterparty = static_cast<std::uint32_t>(contactsPerCp);
            request.generate_identifiers = generateIdentifiers;
            request.seed = seed;

            auto resp = clientManager->process_authenticated_request(std::move(request),
                                                                     std::chrono::minutes(10));

            if (!resp) {
                result.error_message = "Failed to communicate with synthetic service";
                return result;
            }
            if (!resp->success) {
                result.error_message = resp->error_message;
                return result;
            }
            result.success = true;
            result.parties_generated = resp->parties_count;
            return result;
        });

    watcher->setFuture(future);
}

// Phase 3: associate the tenant admin account with all Operational parties.
void TenantExecutePage::startPartyAssociation() {
    ClientManager* clientManager = wizard_->clientManager();

    BOOST_LOG_SEV(lg(), info) << "Phase 3: associating tenant admin with parties";

    struct AssocResult {
        bool success = false;
        std::string error_message;
        int parties_linked = 0;
    };

    auto* watcher = new QFutureWatcher<AssocResult>(this);
    connect(watcher, &QFutureWatcher<AssocResult>::finished, this, [this, watcher]() {
        AssocResult result;
        try {
            result = watcher->result();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Phase 3 async task threw: " << e.what();
            result.error_message = e.what();
        }
        watcher->deleteLater();

        if (!result.success) {
            markFailed(QString::fromStdString(result.error_message));
            return;
        }

        partiesLinked_ = result.parties_linked;
        BOOST_LOG_SEV(lg(), info) << "Party association complete: " << partiesLinked_
                                  << " parties linked";
        appendLog(tr("Admin associated with %1 operational parties.").arg(partiesLinked_));
        wizard_->setPartiesLinkedCount(partiesLinked_);

        statusLabel_->setText(tr("Finalizing tenant setup..."));
        startFinalize();
    });

    QFuture<AssocResult> future =
        QtConcurrent::run([clientManager, publishedBy = publishedBy_]() -> AssocResult {
            AssocResult result;

            const auto accountId = clientManager->accountId();
            if (!accountId) {
                result.success = true;
                return result;
            }

            refdata::messaging::get_parties_request partyReq;
            partyReq.offset = 0;
            partyReq.limit = 1000;
            auto partyResult = clientManager->process_authenticated_request(std::move(partyReq));

            if (!partyResult) {
                result.success = true; // non-fatal: skip association
                return result;
            }

            iam::messaging::save_account_party_request apReq;
            for (const auto& party : partyResult->parties) {
                if (party.party_category != "Operational")
                    continue;
                iam::domain::account_party ap;
                ap.account_id = *accountId;
                ap.party_id = party.id;
                ap.tenant_id = party.tenant_id.to_string();
                ap.modified_by = publishedBy;
                ap.performed_by = publishedBy;
                ap.change_reason_code = std::string(reason::codes::new_record);
                ap.change_commentary = "Tenant provisioning: tenant admin associated with party";
                apReq.account_parties.push_back(std::move(ap));
            }

            if (!apReq.account_parties.empty()) {
                auto apResult = clientManager->process_authenticated_request(std::move(apReq));
                if (apResult && apResult->success)
                    result.parties_linked = static_cast<int>(apReq.account_parties.size());
            }

            result.success = true;
            return result;
        });

    watcher->setFuture(future);
}

// Phase 4: clear bootstrap flag and mark tenant active (blocking, runs on thread pool).
void TenantExecutePage::startFinalize() {
    ClientManager* clientManager = wizard_->clientManager();

    BOOST_LOG_SEV(lg(), info) << "Phase 4: clearing bootstrap flag";

    struct FinalizeResult {
        bool success = false;
        std::string error_message;
    };

    auto* watcher = new QFutureWatcher<FinalizeResult>(this);
    connect(watcher, &QFutureWatcher<FinalizeResult>::finished, this, [this, watcher]() {
        FinalizeResult result;
        try {
            result = watcher->result();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Phase 4 async task threw: " << e.what();
            result.error_message = e.what();
        }
        watcher->deleteLater();

        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        if (!result.success) {
            // Warn but don't block — bootstrap clearing is best-effort.
            BOOST_LOG_SEV(lg(), warn) << "Bootstrap clear failed: " << result.error_message;
            appendLog(tr("Warning: failed to activate organisation: %1")
                          .arg(QString::fromStdString(result.error_message)));
        } else {
            appendLog(tr("Organisation activated successfully."));
        }

        statusLabel_->setText(tr("Setup complete!"));
        statusLabel_->setStyleSheet("font-weight: bold; color: #228B22;");
        progressBar_->setStyleSheet("QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
                                    "background: #2d2d2d; height: 20px; }"
                                    "QProgressBar::chunk { background-color: #228B22; }");

        allSuccess_ = true;
        allComplete_ = true;
        emit completeChanged();
    });

    QFuture<FinalizeResult> future = QtConcurrent::run([clientManager]() -> FinalizeResult {
            FinalizeResult result;

            // complete_tenant_provisioning_command clears system.bootstrap_mode
            // (and sets onboarding.tenant = true) server-side — see
            // clearBootstrapFlag() above for why no separate
            // save_setting_request is issued here.
            iam::messaging::complete_tenant_provisioning_command activateReq;
            auto activateResult =
                clientManager->process_authenticated_request(std::move(activateReq));
            if (!activateResult || !activateResult->success) {
                result.error_message += " / tenant activation failed: " +
                                        (activateResult ? activateResult->message : "no response");
                return result;
            }

            result.success = true;
            return result;
        });

    watcher->setFuture(future);
}

// ============================================================================
// TenantApplyAndSummaryPage
// ============================================================================

TenantApplyAndSummaryPage::TenantApplyAndSummaryPage(TenantProvisioningWizard* wizard)
    : QWizardPage(wizard)
    , wizard_(wizard) {

    setTitle(tr("Setup Complete"));
    setFinalPage(true);

    setupUI();
}

void TenantApplyAndSummaryPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* headerLabel = new QLabel(tr("Tenant setup complete"), this);
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

    auto* logoutBox = new QGroupBox(tr("Action Required"), this);
    auto* logoutLayout = new QVBoxLayout(logoutBox);
    auto* logoutLabel =
        new QLabel(tr("<b>You must log out and log back in to continue.</b><br><br>"
                      "After logging back in, select a party from the party selector. "
                      "The party setup wizard will start automatically for each party "
                      "that has not yet been configured."),
                   this);
    logoutLabel->setWordWrap(true);
    logoutLabel->setTextFormat(Qt::RichText);
    logoutLayout->addWidget(logoutLabel);
    layout->addWidget(logoutBox);
}

void TenantApplyAndSummaryPage::initializePage() {
    QString summary = tr("<p>Your organisation has been onboarded successfully.</p>");

    if (!wizard_->selectedBundleCode().isEmpty()) {
        summary += tr("<p><b>Reference data loaded:</b> %1</p>").arg(wizard_->selectedBundleName());
    }

    const int partiesLinked = wizard_->partiesLinkedCount();
    if (partiesLinked > 0) {
        summary += tr("<p><b>Trading entities created:</b> %1 "
                      "(your administrator account has access to all of them)</p>")
                       .arg(partiesLinked);
    }

    summary += tr("<p>Your organisation is now active and ready for use.</p>");

    summaryLabel_->setText(summary);

    emit wizard_->provisioningCompleted();
}

}
