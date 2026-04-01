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
#include "ores.qt/PartyProvisioningWizard.hpp"
#include "ores.qt/FontUtils.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WidgetUtils.hpp"

#include <array>
#include <chrono>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGroupBox>
#include <QListWidgetItem>
#include <QPushButton>
#include <QSizePolicy>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.database/domain/change_reason_constants.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.reporting.api/domain/report_definition_template.hpp"
#include "ores.variability.api/domain/system_setting.hpp"
#include "ores.variability.api/messaging/system_settings_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;
namespace reason = ores::database::domain::change_reason_constants;

// ============================================================================
// PartyProvisioningWizard
// ============================================================================

PartyProvisioningWizard::PartyProvisioningWizard(
    ClientManager* clientManager,
    QWidget* parent)
    : QWizard(parent),
      clientManager_(clientManager) {

    setWindowTitle(tr("Party Setup"));
    setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::BuildingSkyscraper, IconUtils::DefaultIconColor));
    setMinimumSize(900, 700);
    resize(900, 700);

    setWizardStyle(QWizard::ModernStyle);
    setOption(QWizard::NoBackButtonOnStartPage, true);
    setOption(QWizard::NoBackButtonOnLastPage, true);
    setOption(QWizard::NoCancelButtonOnLastPage, true);

    setupPages();
}

void PartyProvisioningWizard::setupPages() {
    WidgetUtils::setupComboBoxes(this);
    setPage(Page_Welcome,          new PartyWelcomePage(this));
    setPage(Page_CounterpartySetup, new PartyCounterpartySetupPage(this));
    setPage(Page_OrganisationSetup, new PartyOrganisationSetupPage(this));
    setPage(Page_ReportSetup,      new PartyReportSetupPage(this));
    setPage(Page_ReportInstall,    new PartyReportInstallPage(this));
    setPage(Page_Summary,          new PartyApplyAndSummaryPage(this));

    setStartId(Page_Welcome);
}

bool PartyProvisioningWizard::markPartyActive() {
    BOOST_LOG_SEV(lg(), info) << "Setting current party status to Active";

    const auto party_id = clientManager_->currentPartyId();

    refdata::messaging::get_parties_request list_req;
    list_req.offset = 0;
    list_req.limit = 1000;
    auto list_result = clientManager_->process_authenticated_request(std::move(list_req));
    if (!list_result) {
        BOOST_LOG_SEV(lg(), warn) << "markPartyActive: failed to fetch parties: "
                                  << list_result.error();
        return false;
    }

    refdata::domain::party party;
    bool found = false;
    for (const auto& p : list_result->parties) {
        if (p.id == party_id) {
            party = p;
            found = true;
            break;
        }
    }
    if (!found) {
        BOOST_LOG_SEV(lg(), warn) << "markPartyActive: party not found in list";
        return false;
    }

    party.status = "Active";
    party.change_commentary = "Party setup wizard completed";

    refdata::messaging::save_party_request save_req;
    save_req.data = std::move(party);
    auto save_result = clientManager_->process_authenticated_request(std::move(save_req));
    if (!save_result) {
        BOOST_LOG_SEV(lg(), warn) << "markPartyActive: failed to save party: "
                                  << save_result.error();
        return false;
    }
    if (!save_result->success) {
        BOOST_LOG_SEV(lg(), warn) << "markPartyActive: save_party failed: "
                                  << (save_result->message.empty()
                                      ? "Unknown error" : save_result->message);
        return false;
    }
    BOOST_LOG_SEV(lg(), info) << "Party status set to Active successfully";
    return true;
}

// ============================================================================
// PartyWelcomePage
// ============================================================================

PartyWelcomePage::PartyWelcomePage(PartyProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Welcome"));
    setupUI();
}

void PartyWelcomePage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* welcomeLabel = new QLabel(
        tr("Welcome to the party setup wizard."), this);
    welcomeLabel->setStyleSheet("font-size: 16pt; font-weight: bold;");
    welcomeLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(welcomeLabel);

    layout->addSpacing(10);

    auto* descLabel = new QLabel(this);
    descLabel->setWordWrap(true);
    descLabel->setText(
        tr("This wizard sets up the operational structure for this party. "
           "The party hierarchy already exists — this wizard will import "
           "counterparties and configure the organisational structure.\n\n"
           "The setup process includes:"));
    layout->addWidget(descLabel);

    auto* stepsLabel = new QLabel(this);
    stepsLabel->setWordWrap(true);
    stepsLabel->setTextFormat(Qt::RichText);
    stepsLabel->setText(
        tr("<ol>"
           "<li><b>Counterparty Setup</b> - Select dataset size and import "
           "counterparties from the GLEIF LEI registry.</li>"
           "<li><b>Organisation Setup</b> - Publish business units, "
           "portfolios, and trading books.</li>"
           "<li><b>Report Definitions</b> - Optionally create a set of "
           "standard risk report definitions.</li>"
           "</ol>"));
    layout->addWidget(stepsLabel);

    layout->addStretch();

    auto* noteBox = new QGroupBox(tr("Note"), this);
    auto* noteLayout = new QVBoxLayout(noteBox);
    auto* noteLabel = new QLabel(
        tr("You can skip this setup by clicking Cancel. You can configure "
           "counterparties and reports manually from the application menus."),
        this);
    noteLabel->setWordWrap(true);
    noteLayout->addWidget(noteLabel);
    layout->addWidget(noteBox);
}

// ============================================================================
// PartyCounterpartySetupPage
// ============================================================================

PartyCounterpartySetupPage::PartyCounterpartySetupPage(
    PartyProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Counterparty Import"));
    setSubTitle(tr("Select the GLEIF dataset size to use for counterparty import."));
    setupUI();
}

void PartyCounterpartySetupPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* infoLabel = new QLabel(this);
    infoLabel->setWordWrap(true);
    infoLabel->setText(
        tr("Counterparties represent the external entities your organisation "
           "trades with or has business relationships with.\n\n"
           "Counterparties are imported from the full GLEIF LEI registry dataset. "
           "Select the dataset size to use for the import.\n\n"
           "You can also add counterparties manually from the Counterparties "
           "window after completing this wizard."));
    infoLabel->setTextFormat(Qt::RichText);
    layout->addWidget(infoLabel);

    auto* sizeLayout = new QHBoxLayout();
    sizeLayout->addWidget(new QLabel(tr("Dataset size:"), this));
    datasetSizeCombo_ = new QComboBox(this);
    datasetSizeCombo_->addItem(tr("Large (~15,000 entities)"), QStringLiteral("large"));
    datasetSizeCombo_->addItem(tr("Small (~6,000 entities)"),  QStringLiteral("small"));
    sizeLayout->addWidget(datasetSizeCombo_);
    sizeLayout->addStretch();
    layout->addLayout(sizeLayout);

    layout->addStretch();
}

bool PartyCounterpartySetupPage::validatePage() {
    wizard_->setLeiDatasetSize(datasetSizeCombo_->currentData().toString());
    return true;
}

// ============================================================================
// PartyOrganisationSetupPage
// ============================================================================

PartyOrganisationSetupPage::PartyOrganisationSetupPage(
    PartyProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Organisation Setup"));
    setSubTitle(tr("Importing counterparties and publishing organisation structure."));
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

bool PartyOrganisationSetupPage::isComplete() const {
    return publishComplete_;
}

void PartyOrganisationSetupPage::initializePage() {
    publishComplete_ = false;
    publishSuccess_ = false;
    logOutput_->clear();
    progressBar_->setRange(0, 0);
    progressBar_->setStyleSheet(
        "QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
        "background: #2d2d2d; height: 20px; }"
        "QProgressBar::chunk { background-color: #4a9eff; }");

    statusLabel_->setText(tr("Importing counterparties and organisation data..."));
    startPublish();
}

void PartyOrganisationSetupPage::appendLog(const QString& message) {
    logOutput_->append(message);
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void PartyOrganisationSetupPage::startPublish() {
    const std::string publishedBy = wizard_->clientManager()->currentUsername();
    ClientManager* clientManager = wizard_->clientManager();
    const std::string selectedBundle = wizard_->selectedBundleCode().toStdString();
    const std::string datasetSize = wizard_->leiDatasetSize().toStdString();
    const std::string size = datasetSize.empty() ? "small" : datasetSize;

    BOOST_LOG_SEV(lg(), info) << "Publishing counterparties (dataset: " << size
                              << ") and organisation structure";

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
                << result->datasets_succeeded << " datasets, "
                << result->total_records_inserted << " inserted, "
                << result->total_records_updated << " updated";
            statusLabel_->setText(tr("Organisation setup complete!"));
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

    // Run both publishes sequentially on a background thread
    QFuture<std::optional<ResponseType>> future = QtConcurrent::run(
        [clientManager, publishedBy, selectedBundle, size]()
            -> std::optional<ResponseType> {

            // Step 1: Re-publish the selected bundle opting in counterparties only.
            // No root_lei filtering — counterparties are the full GLEIF dataset.
            dq::messaging::publish_bundle_params leiParams;
            leiParams.opted_in_datasets.push_back(
                "gleif.lei_counterparties." + size);
            const std::string leiParamsJson =
                dq::messaging::build_params_json(leiParams);

            dq::messaging::publish_bundle_request leiRequest;
            leiRequest.bundle_code = selectedBundle;
            leiRequest.mode = dq::domain::publication_mode::upsert;
            leiRequest.published_by = publishedBy;
            leiRequest.atomic = true;
            leiRequest.params_json = leiParamsJson;

            auto leiResult = clientManager->process_authenticated_request(
                std::move(leiRequest), std::chrono::minutes(5));

            if (!leiResult) {
                return std::nullopt;
            }
            if (!leiResult->success) {
                return *leiResult;
            }

            // Step 2: Publish organisation bundle (business units, portfolios, books)
            // Pass the current party_id so publish functions scope data to this
            // party rather than resolving the tenant's root party.
            dq::messaging::publish_bundle_params orgParams;
            orgParams.party_id = boost::uuids::to_string(
                clientManager->currentPartyId());
            dq::messaging::publish_bundle_request orgRequest;
            orgRequest.bundle_code = "organisation";
            orgRequest.mode = dq::domain::publication_mode::upsert;
            orgRequest.published_by = publishedBy;
            orgRequest.atomic = true;
            orgRequest.params_json = dq::messaging::build_params_json(orgParams);

            auto orgResult = clientManager->process_authenticated_request(
                std::move(orgRequest), std::chrono::minutes(5));

            if (!orgResult) {
                return std::nullopt;
            }
            return *orgResult;
        }
    );

    watcher->setFuture(future);

    appendLog(tr("[1/2] Importing GLEIF counterparties (dataset: %1)...")
        .arg(QString::fromStdString(size)));
    appendLog(tr("[2/2] Publishing organisation structure (business units, "
                  "portfolios, books)..."));
}

// ============================================================================
// PartyReportSetupPage
// ============================================================================

PartyReportSetupPage::PartyReportSetupPage(PartyProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Report Definitions"));
    setSubTitle(tr("Optionally create a set of standard risk report definitions "
                   "for your party. You can add, modify, or remove these later "
                   "from the Reporting menu."));

    setupUI();
}

void PartyReportSetupPage::setupUI() {
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(8);

    auto* infoLabel = new QLabel(
        tr("Select the report definitions to create. All reports are "
           "scheduled on weekdays and use the 'skip' concurrency policy "
           "(new runs are skipped while a prior run is still in progress)."),
        this);
    infoLabel->setWordWrap(true);
    layout->addWidget(infoLabel);

    layout->addSpacing(6);

    // Select All / Deselect All buttons
    auto* btnLayout = new QHBoxLayout();
    auto* selectAllBtn = new QPushButton(tr("Select All"), this);
    auto* deselectAllBtn = new QPushButton(tr("Deselect All"), this);
    selectAllBtn->setMaximumWidth(120);
    deselectAllBtn->setMaximumWidth(120);
    btnLayout->addWidget(selectAllBtn);
    btnLayout->addWidget(deselectAllBtn);
    btnLayout->addStretch();
    layout->addLayout(btnLayout);

    loadingLabel_ = new QLabel(tr("Loading report templates..."), this);
    loadingLabel_->setAlignment(Qt::AlignCenter);
    layout->addWidget(loadingLabel_);

    errorLabel_ = new QLabel(this);
    errorLabel_->setWordWrap(true);
    errorLabel_->setStyleSheet("color: #cc4444;");
    errorLabel_->hide();
    layout->addWidget(errorLabel_);

    reportList_ = new QListWidget(this);
    reportList_->setSpacing(2);
    reportList_->setAlternatingRowColors(true);
    reportList_->hide();
    layout->addWidget(reportList_);

    connect(selectAllBtn, &QPushButton::clicked, this, [this]() {
        for (int i = 0; i < reportList_->count(); ++i) {
            reportList_->item(i)->setCheckState(Qt::Checked);
        }
    });

    connect(deselectAllBtn, &QPushButton::clicked, this, [this]() {
        for (int i = 0; i < reportList_->count(); ++i) {
            reportList_->item(i)->setCheckState(Qt::Unchecked);
        }
    });
}

void PartyReportSetupPage::initializePage() {
    loadingLabel_->show();
    errorLabel_->hide();
    reportList_->clear();
    reportList_->hide();
    loadTemplates();
}

void PartyReportSetupPage::loadTemplates() {
    using ResponseType = reporting::messaging::get_report_definition_templates_response;
    ClientManager* clientManager = wizard_->clientManager();

    auto* watcher = new QFutureWatcher<std::optional<ResponseType>>(this);
    connect(watcher, &QFutureWatcher<std::optional<ResponseType>>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        loadingLabel_->hide();

        if (!result || !result->success) {
            const QString errMsg = result
                ? QString::fromStdString(result->message)
                : tr("No response from reporting service.");
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to load report definition templates: "
                << errMsg.toStdString();
            errorLabel_->setText(tr("Failed to load templates: %1").arg(errMsg));
            errorLabel_->show();
            return;
        }

        populateList(result->templates);
        reportList_->show();
    });

    QFuture<std::optional<ResponseType>> future = QtConcurrent::run(
        [clientManager]() -> std::optional<ResponseType> {
            reporting::messaging::get_report_definition_templates_request req;
            auto r = clientManager->process_authenticated_request(std::move(req));
            if (!r) return std::nullopt;
            return std::move(*r);
        });
    watcher->setFuture(future);
}

void PartyReportSetupPage::populateList(
    const std::vector<ores::reporting::domain::report_definition_template>& templates) {

    reportList_->clear();
    for (const auto& t : templates) {
        auto* item = new QListWidgetItem(reportList_);
        item->setCheckState(Qt::Checked);
        item->setText(QString("%1\n%2")
            .arg(QString::fromStdString(t.name))
            .arg(QString::fromStdString(t.description)));
        item->setData(Qt::UserRole,     QString::fromStdString(t.name));
        item->setData(Qt::UserRole + 1, QString::fromStdString(t.description));
        item->setData(Qt::UserRole + 2, QString::fromStdString(t.schedule_expression));
        item->setData(Qt::UserRole + 3, QString::fromStdString(t.report_type));
        item->setData(Qt::UserRole + 4, QString::fromStdString(t.concurrency_policy));
        reportList_->addItem(item);
    }
}

bool PartyReportSetupPage::validatePage() {
    std::vector<PartyProvisioningWizard::ReportSpec> selected;
    for (int i = 0; i < reportList_->count(); ++i) {
        const auto* item = reportList_->item(i);
        if (item->checkState() == Qt::Checked) {
            PartyProvisioningWizard::ReportSpec spec;
            spec.name = item->data(Qt::UserRole).toString().toStdString();
            spec.description = item->data(Qt::UserRole + 1).toString().toStdString();
            spec.schedule_expression = item->data(Qt::UserRole + 2).toString().toStdString();
            spec.report_type = item->data(Qt::UserRole + 3).toString().toStdString();
            spec.concurrency_policy = item->data(Qt::UserRole + 4).toString().toStdString();
            selected.push_back(std::move(spec));
        }
    }
    wizard_->setSelectedReports(std::move(selected));
    return true;
}

int PartyReportSetupPage::nextId() const {
    int checked = 0;
    for (int i = 0; i < reportList_->count(); ++i) {
        if (reportList_->item(i)->checkState() == Qt::Checked) {
            ++checked;
        }
    }
    return checked > 0
        ? PartyProvisioningWizard::Page_ReportInstall
        : PartyProvisioningWizard::Page_Summary;
}

// ============================================================================
// PartyReportInstallPage
// ============================================================================

PartyReportInstallPage::PartyReportInstallPage(PartyProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Creating Report Definitions"));
    setFinalPage(false);

    auto* layout = new QVBoxLayout(this);

    statusLabel_ = new QLabel(tr("Starting..."), this);
    statusLabel_->setStyleSheet("font-weight: bold;");
    layout->addWidget(statusLabel_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0);
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

bool PartyReportInstallPage::isComplete() const {
    return installComplete_;
}

void PartyReportInstallPage::initializePage() {
    installComplete_ = false;
    installSuccess_ = false;
    logOutput_->clear();
    statusLabel_->setText(tr("Creating report definitions..."));
    progressBar_->setRange(0, 0);
    progressBar_->setStyleSheet(
        "QProgressBar { border: 1px solid #3d3d3d; border-radius: 3px; "
        "background: #2d2d2d; height: 20px; }"
        "QProgressBar::chunk { background-color: #4a9eff; }");

    startInstall();
}

void PartyReportInstallPage::appendLog(const QString& message) {
    logOutput_->append(message);
    auto cursor = logOutput_->textCursor();
    cursor.movePosition(QTextCursor::End);
    logOutput_->setTextCursor(cursor);
}

void PartyReportInstallPage::startInstall() {
    const auto specs = wizard_->selectedReports();
    const std::string username = wizard_->clientManager()->currentUsername();
    ClientManager* clientManager = wizard_->clientManager();

    BOOST_LOG_SEV(lg(), info) << "Creating " << specs.size()
                              << " report definitions";

    appendLog(tr("Looking up system party..."));

    struct InstallResult {
        bool success = false;
        int created = 0;
        std::string error;
    };

    auto* watcher = new QFutureWatcher<InstallResult>(this);
    connect(watcher, &QFutureWatcher<InstallResult>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        progressBar_->setRange(0, 1);
        progressBar_->setValue(1);

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Report install failed: "
                                       << result.error;
            statusLabel_->setText(tr("Failed to create report definitions."));
            appendLog(tr("ERROR: %1").arg(
                QString::fromStdString(result.error)));
            progressBar_->setStyleSheet(
                "QProgressBar::chunk { background-color: #cc0000; }");
            installSuccess_ = false;
        } else {
            BOOST_LOG_SEV(lg(), info) << "Created " << result.created
                                      << " report definitions";
            statusLabel_->setText(
                tr("Created %1 report definition(s) successfully.")
                    .arg(result.created));
            installSuccess_ = true;
        }

        installComplete_ = true;
        emit completeChanged();
    });

    QFuture<InstallResult> future = QtConcurrent::run(
        [clientManager, specs, username]() -> InstallResult {
            InstallResult result;

            // Step 1: find system party
            refdata::messaging::get_parties_request partiesReq;
            partiesReq.limit = 10;
            auto partiesRes = clientManager->process_authenticated_request(
                std::move(partiesReq));

            if (!partiesRes || partiesRes->parties.empty()) {
                result.error = "No parties found; cannot assign report definitions.";
                return result;
            }

            // Use the System-category party (root, no parent). Fall back to first.
            boost::uuids::uuid partyId = partiesRes->parties.front().id;
            for (const auto& p : partiesRes->parties) {
                if (p.party_category == "System") {
                    partyId = p.id;
                    break;
                }
            }

            // Step 2: create each selected report definition
            boost::uuids::random_generator gen;
            namespace reason = ores::database::domain::change_reason_constants;

            for (const auto& spec : specs) {
                reporting::domain::report_definition def;
                def.id = gen();
                def.name = spec.name;
                def.description = spec.description;
                def.report_type = spec.report_type;
                def.schedule_expression = spec.schedule_expression;
                def.concurrency_policy = spec.concurrency_policy;
                def.party_id = partyId;
                def.modified_by = username;
                def.performed_by = username;
                def.change_reason_code =
                    std::string(reason::codes::new_record);
                def.change_commentary =
                    "Created during party provisioning";

                reporting::messaging::save_report_definition_request req;
                req.definition = std::move(def);

                auto res = clientManager->process_authenticated_request(
                    std::move(req));

                if (!res || !res->success) {
                    result.error = "Failed to create '" + spec.name + "': " +
                        (res ? res->message : "no server response");
                    return result;
                }

                ++result.created;
            }

            result.success = true;
            return result;
        }
    );

    watcher->setFuture(future);

    appendLog(tr("Creating %1 report definition(s)...")
        .arg(static_cast<int>(specs.size())));
}

// ============================================================================
// PartyApplyAndSummaryPage
// ============================================================================

PartyApplyAndSummaryPage::PartyApplyAndSummaryPage(
    PartyProvisioningWizard* wizard)
    : QWizardPage(wizard), wizard_(wizard) {

    setTitle(tr("Party Setup Complete"));
    setFinalPage(true);

    setupUI();
}

void PartyApplyAndSummaryPage::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setSpacing(20);

    auto* headerLabel = new QLabel(
        tr("Party setup complete"), this);
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

void PartyApplyAndSummaryPage::initializePage() {
    const bool activated = wizard_->markPartyActive();

    QString summary;
    if (!activated) {
        summary = tr("<p><b>Warning:</b> Could not activate the party — "
                     "the party setup wizard may reappear on your next login. "
                     "Please contact your administrator.</p>");
    } else {
        summary = tr("<p>Your party setup has been completed successfully.</p>");
    }

    if (wizard_->organisationPublished()) {
        summary += tr("<p><b>Organisation data:</b> Counterparties, business units, "
                      "portfolios, and trading books published.</p>");
    }

    const auto& reports = wizard_->selectedReports();
    if (!reports.empty()) {
        summary += tr("<p><b>Report definitions created (%1):</b></p><ul>")
            .arg(static_cast<int>(reports.size()));
        for (const auto& r : reports) {
            summary += tr("<li>%1</li>")
                .arg(QString::fromStdString(r.name));
        }
        summary += tr("</ul>");
    }

    summary += tr("<p>The party setup flag has been cleared. This wizard "
                  "will not appear on your next login.</p>");

    summaryLabel_->setText(summary);

    emit wizard_->provisioningCompleted();
}

} // namespace ores::qt
