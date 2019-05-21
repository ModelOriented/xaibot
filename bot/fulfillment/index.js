// See https://github.com/dialogflow/dialogflow-fulfillment-nodejs
// for Dialogflow fulfillment library docs, samples, and to report issues
'use strict';
 
const functions = require('firebase-functions');
const {WebhookClient} = require('dialogflow-fulfillment');
const {Card, Suggestion, Image, Payload} = require('dialogflow-fulfillment');
const { Carousel } = require('actions-on-google');
const storage_context = 'storage_context';
const all_variables = ['age', 'gender', 'fare', 'class', 'parch', 'sibsp', 'embarked'];
const http = require('http');
const server_address = 'http://52.31.27.158:8787';
const pretty_vars = {
 	'age': 'Age',
  	'gender': 'Gender',
  	'fare': 'Fare',
  	'parch': 'Number of parents/children',
  	'sibsp': 'Number of siblings/spouse',
  	'embarked': 'Place of embarkment',
  	'class': 'Class'
};

function get_var_name(variable) {
 if (variable === "gender_value") return 'gender';
 if (variable === "class_value") return 'class';
 return variable;
}

function get_var_key(variable) {
 if (variable === "gender") return 'gender_value';
 if (variable === "class") return 'class_value';
 return variable;

}

process.env.DEBUG = 'dialogflow:debug'; // enables lib debugging statements
 
exports.dialogflowFirebaseFulfillment = functions.https.onRequest((request, response) => {
  const agent = new WebhookClient({ request, response });
  const parameters = request.body.queryResult.parameters;
  console.log('Dialogflow Request headers: ' + JSON.stringify(request.headers));
  console.log('Dialogflow Request body: ' + JSON.stringify(request.body));
  
  function fallback(agent) {
    agent.add(`Sorry, I don't understand yet. But I'll learn from this conversation and improve in the future!`);
    agent.add(`Click below if you need help`);
    agent.add(new Suggestion('help'));
  }
  
   function variable_explanation(variable, agent) {
        switch(variable) {
            case 'age':
                return 'Age in years.';
            case 'gender':
                return 'Gender either "male" or "female"';
            case 'embarked':
            	return 'Where did the passenger embark. One of ("Belfast", "Cherbourg", "Queenstown", "Southampton")';
          	case 'fare':
            	return 'Ticket fare in pounds.';
          	case 'parch':
            	return 'Number of Parent/Child aboard';
          	case 'sibsp':
            	return 'Number of Sibling/Spouse aboard';
          	case 'class':
            	return 'Passenger class. One of ("1st", "2nd", "3rd", "deck crew", "engineering crew", "restaurant staff" or “victualling crew”)';
            default:
                return `I don't know the variable ${variable}`;
        }
    }

    function explain_feature(agent) {
        let feature = request.body.queryResult.parameters.variable;
      	switch (feature) {
          case 'age':
   			 agent.setContext({'name': 'specify_age', 'lifespan': 1}); break;
          case 'fare':
             agent.setContext({'name': 'specify_fare', 'lifespan': 1}); break;
          case 'parch':
             agent.setContext({'name': 'specify_parch', 'lifespan': 1}); break;
          case 'sibsp':
             agent.setContext({'name': 'specify_sibsp', 'lifespan': 1}); break;
		  case 'class':
             agent.setContext({'name': 'specify_class', 'lifespan': 1}); break;
        }
        let text = variable_explanation(feature, agent);
        agent.add(text);
    }
  

   function predict(agent, params) {
      	let path = `${server_address}/predict?${params}`;
     	console.log(`API path:${path}`);
        return new Promise((resolve, reject) => {
            http.get(path, (res) => {
                let body = ''; // var to store the response chunks
                res.on('data', (d) => { body += d; }); // store each response chunk
                res.on('end', () => {
                    // After all the data has been received parse the JSON for desired data
                    // Resolve the promise with the output text
                  	let survival_chance = JSON.parse(body).result[0].toString();
                  	let res_str = survival_message(survival_chance);
                  	console.log(res_str);
                  	let output = agent.add(res_str);
                    resolve(output);
                });
                res.on('error', (error) => {
                  	console.log('error in API call');
                    reject();
                });
            });
        });
    }

   function age(agent) {
     let age_val = parameters.number;
     let age_num = parseInt(age_val);
     if (age_num < 0) {
       agent.add(`I don't really think you are ${age_val} years old. Tell me your real age.`);
       return;
     }
     let params = formatted_parameters('age', age_val);
	 set_var_value(agent, 'age', age_val);
     console.log(params);
     return predict(agent, params);
  }
  
  function multi_slot_filling(agent) {
     let age_val = parameters.number;
     let params_dict = {};
     if (age_val) { params_dict['age'] = age_val; }
     let gender_val = parameters.gender;
     if (gender_val && gender_val !== "") { params_dict['gender_value'] = gender_val; }
     let embarked_val = parameters.embarkment_place;
     if (embarked_val && embarked_val !== "") { params_dict['embarked'] = embarked_val; }
	 let class_val = parameters.class;
	 if (class_val && class_val !== "") { params_dict['class_value'] = class_val; }
     console.log(params_dict);
     console.log(JSON.stringify(params_dict));
	 set_multiple_var(agent, params_dict);
	 let params = formatted_params_dict(params_dict);
     return predict(agent, params);
  }
  
   function setting_fare(agent) {
     let fare_val = parameters.number;
     let params = formatted_parameters('fare', fare_val);
     set_var_value(agent, 'fare', fare_val);
     return predict(agent, params);
  }

  
  function gender(agent) {
    let gender_val = parameters.gender;
    let params = formatted_parameters('gender_value', gender_val);
	set_var_value(agent, 'gender_value', gender_val);
    return predict(agent, params);
  }
  
  function setting_sibsp(agent) {
    let sibsp_val = parameters.number;
    if (!sibsp_val) {
      	agent.add(`I'm sorry. I'm not sure. How many siblings and spouse altogether you travelled with?`);
      	agent.setContext({'name': 'specify_sibsp', 'lifespan': 1});
    }
    else {
      	agent.add(`I understood you travelled with ${sibsp_val} siblings and spouse altogether`);
        let params = formatted_parameters('sibsp', sibsp_val);
     	set_var_value(agent, 'sibsp', sibsp_val);
    	return predict(agent, params);
    }
  }

  function setting_parch(agent) {
    let parch_val = parameters.number;
    if (!parch_val) {
      	agent.add(`I'm sorry. I'm not sure. How many parents and children altogether you travelled with?`);
      	agent.setContext({'name': 'specify_parch', 'lifespan': 1});
    }
    else {
      	agent.add(`I understood you travelled with ${parch_val} parents and children altogether`);
        let params = formatted_parameters('parch', parch_val);
     	set_var_value(agent, 'parch', parch_val);
    	return predict(agent, params);
    }
  }
  
  function specify_sibsp(agent) {
    let sibsp_val = parameters.number;
    let params = formatted_parameters('sibsp', sibsp_val);
    set_var_value(agent, 'sibsp', sibsp_val);
    return predict(agent, params);
  }
  
  function specify_parch(agent) {
	let parch_val = parameters.number;
    let params = formatted_parameters('parch', parch_val);
    set_var_value(agent, 'parch', parch_val);
    return predict(agent, params);
  }
  
  function specify_age(agent) {
	let age_val = parameters.number;
    let params = formatted_parameters('age', age_val);
    set_var_value(agent, 'age', age_val);
    return predict(agent, params);
  }
  
  function specify_fare(agent) {
	let fare_val = parameters.number;
    let params = formatted_parameters('fare', fare_val);
    set_var_value(agent, 'fare', fare_val);
    return predict(agent, params);
  }
  
  function travelling_alone(agent) {
    agent.add(`I understand you travelled alone. I'm setting sibsp and parch to zero.`);
    let params = formatted_parameters('parch', '0', 'sibsp', '0');
    set_var_value(agent, 'parch', '0', 'sibsp', '0');
    return predict(agent, params);

  }
  
  function setting_embarked(agent) {
    let embarked_val = parameters.embarkment_place;
    console.log(embarked_val);
    if (!embarked_val) {
      	agent.add("Where have you embarked on the Titanic? Possible places were:");
     	["Belfast", "Cherbourg", "Queenstown", "Southampton"].forEach(place => agent.add(new Suggestion(place)));
    }
    else {
      	embarked_val = embarked_val;
        let params = formatted_parameters('embarked', embarked_val);
     	set_var_value(agent, 'embarked', embarked_val); 
    	return predict(agent, params);
    }
  }
  
  function setting_class(agent) {
   	let class_val = parameters.class;
    if (!class_val && request.body.queryResult.queryText == "passenger") {
      	["1st", "2nd", "3rd"].forEach(kind => agent.add(new Suggestion(kind)));
    }
    else if (!class_val && request.body.queryResult.queryText == "crew") {
      	["deck crew", "engineering crew", "restaurant staff", "victualling crew"].forEach(kind =>
                                            agent.add(new Suggestion(kind)));
    }
    else if (!class_val) {
      	agent.add("Were you travelling as a passenger or part of the crew?");
      	["passenger", "crew"].forEach(kind => agent.add(new Suggestion(kind)));
    }
    else {
      	class_val = class_val;
        let params = formatted_parameters('class_value', class_val);
     	set_var_value(agent, 'class_value', class_val); 
    	return predict(agent, params);
    }
  }

  function clear_variable(agent) {
   let variable = parameters.variable;
   console.log(variable);
   set_var_value(agent, get_var_key(variable), 'X');
   agent.add(`Variable ${variable} was cleared`);
   agent.add(new Suggestion('passenger details'));
   agent.add(new Suggestion('survival chance'));
  }
  
  function set_var_value(agent, variable, value, var2=null, val2=null) {
     let context_dict = {
       'name': storage_context,
       'lifespan': 100,
       'parameters': {
       }
     };
    context_dict.parameters[variable] = value;
    if (var2 !== null) {
     	context_dict.parameters[var2] = val2; 
    }
    agent.setContext(context_dict);
  }
  
  function set_multiple_var(agent, data_dict) {
    let context_dict = {
       'name': storage_context,
       'lifespan': 100,
       'parameters': {
       }
     };
    console.log(data_dict);
    Object.keys(data_dict).forEach(variable => context_dict.parameters[variable] = data_dict[variable]);
    agent.setContext(context_dict);
  }

  
  function get_var_value(agent, variable) {
    let context = agent.getContext(storage_context);
    if (!context || context.parameters[variable] === null || context.parameters[variable] === undefined) {
      return 'X';
    } 
    else {
      return context.parameters[variable]; 
    }
  }
  
  function current_knowledge(agent) {
    	let unknown = [];
    	all_variables.forEach(variable => {
          		let val = get_var_value(agent, get_var_key(variable));
          		if (val === null || val == 'X' || val === undefined || val === "") {
                 	agent.add(`${pretty_vars[variable]} is not defined`);
                }
          		else {
                 	agent.add(`${pretty_vars[variable]}: ${val}`); 
                }
        	}
          );
  }
  
  function survival_message(probability) {
   	if (probability < 0.4) {
     	return `I'm sorry. It looks like you would've died on Titanic. Your chance of survival equals ${probability}`;
    }
    else if (probability < 0.6) {
      	return `Your chance of survival equals ${probability}. It's close to a toss of a coin!`;
    }
    else {
    	return `Good news! You would've survived the disaster. Your chance of survival equals ${probability}`;
    }
  }
  
  function current_prediction(agent) {
    	let params = formatted_parameters();
    	return predict(agent, params);
  }
  
  function formatted_parameters(changed_variable=null, changed_value=null, changed_var2=null, changed_val2=null) {
	let params_str = ``;
    let params_dict = new Map();
    all_variables.forEach(variable => params_dict[get_var_key(variable)] = get_var_value(agent, get_var_key(variable)));
    if (changed_variable) { 
      params_dict[changed_variable] = changed_value;
    }
    if (changed_var2) {
      params_dict[changed_var2] = changed_val2; 
    }
    for (var key in params_dict) { 
    	params_str += get_var_name(key) + `=` + params_dict[key] + `&`;
    }
    console.log(params_str);
    return params_str;
  }
  
  function formatted_params_dict(new_params_dict) {
    let params_str = ``;
    let params_dict = new Map();
    all_variables.forEach(variable => params_dict[get_var_key(variable)] = get_var_value(agent, get_var_key(variable)));
	Object.keys(new_params_dict).forEach(variable => params_dict[variable] = new_params_dict[variable]);
       
    for (var key in params_dict) { 
    	params_str += get_var_name(key) + `=` + params_dict[key] + `&`;
    }
    console.log(params_str);
    return params_str;

  }
  
  function ceteris_paribus(agent) {
     let variable = parameters.variable;
     if (!variable || variable.length === 0) 
     { 
       variable = 'age'; 
     }
     else {
       variable = variable[0]; 
     }
     let params = formatted_parameters();
     let imageUrl = `${server_address}/ceteris_paribus?${params}variable=${variable}`;
     console.log(imageUrl);
     
   	 agent.add(`Creating a plot. It may take a few seconds...`);
     agent.add(new Card({
       title: `Ceteris Paribus plot`,
        imageUrl: imageUrl,
        //text: `This plot illustrates how the prediction changes when ${variable} is changed and everything else is fixed`,
        buttonText: `See larger plot`,
        buttonUrl: imageUrl
    	})
  	);
  }
  
  function how_to_survive(agent) {
    parameters.variable = ["class"];
    console.log(parameters);
    // TODO more detailed analysis here
    ceteris_paribus(agent);
    agent.add(`Travelling in a different class might increase your survival chance`);
    agent.add(`You might also ask what-if questions for other variables`);
  }
  
  function break_down(agent) {
     let params = formatted_parameters();
     let imageUrl = `${server_address}/break_down?${params}`;
     console.log(imageUrl);
     
   	 agent.add(`Creating a plot. It may take a few seconds...`);
     agent.add(new Card({
       title: `Break down plot`,
        imageUrl: imageUrl,
        text: `This chart illustrates the contribution of variables to the final prediction`,
        buttonText: `See larger plot`,
        buttonUrl: imageUrl
    	})
  	);

  }

   function list_variables(agent) {
     let first = 4;
     if (request.body.queryResult.queryText == "More...") {
      	 all_variables.slice(first).forEach(variable => agent.add(new Suggestion(variable)));
     }
     else {
       	agent.add(`Click on the variable to see a detailed description`);
     	all_variables.slice(0, first).forEach(variable => agent.add(new Suggestion(variable)));
       	agent.add(new Suggestion('More...'));
     }
   }
  
  function welcome(agent) {
    agent.add(`Hello! I'm DrAnt, a Titanic survival bot. Let's see whether you would've survived on Titanic and discuss the model predictions.`);
	//agent.add(`You can list variables, ask about their meanings and set values at any time.`);
    //agent.add(`Do not limit yourself. Ask anything you'd like to know. I learn from interactions like this!`);
    agent.add(new Image(`https://upload.wikimedia.org/wikipedia/en/b/bb/Titanic_breaks_in_half.jpg`));
    agent.add(`You might start by telling some details about you`);
    agent.add(`List variables and ask about their meaning at any time:`);
    agent.add(new Suggestion(`list variables`));
    agent.add(new Suggestion(`describe the problem`));
    agent.add(`You might also start as Jack or Rose from the movie :)`);
    agent.add(new Image(
      `https://vignette.wikia.nocookie.net/jamescameronstitanic/images/b/b9/Roseandjack.jpg/revision/latest?cb=20110213201351`));
    agent.add(new Suggestion('Jack'));
    agent.add(new Suggestion('Rose'));
  }
  
  function restart(agent) {
	agent.setContext({'name': storage_context, 'lifespan': '0'});
    agent.add(`Let's start from the beginning!`);
  }
  
  function reset_rose(agent) {
    restart(agent);
  }
  
  function reset_jack(agent) {
    restart(agent);
  }
  
  function end_conversation(agent) {
   	agent.add('Bye :( Great talking to you! Come back later, as I will improve!');
    agent.add(new Image(`https://vignette.wikia.nocookie.net/jamescameronstitanic/images/5/55/Jack_and_Rose-2.jpg/revision/latest?cb=20120405074438`));
	agent.setContext({'name': storage_context, 'lifespan': '0'});
  }
  
  function help_needed(agent) {
    agent.add(new Suggestion(`list all variables`));
    agent.add(new Suggestion(`describe the problem`));
    agent.add(new Suggestion(`what do you know about me?`));
  }
  
  function problem_setting(agent) {
    let wiki_url = `https://en.wikipedia.org/wiki/Passengers_of_the_RMS_Titanic`;
    let image_url = `https://natgeo.imgix.net/factsheets/thumbnails/RMSTitanic_TimelineofDisaster_Titanic.jpg?auto=compress,format&w=1024&h=560&fit=crop`;
    agent.add(new Card({
       	title: `Titanic disaster`,
        imageUrl: image_url,
        buttonText: `Read more...`,
      	buttonUrl: wiki_url
    	})
  	);
  }
  
  function jack_dawson(agent) {
    let jack_dict = {
      'age': '20',
      'gender_value': 'male',
      'embarked': 'Southampton',
      'sibsp': '0',
      'parch': '0',
      'class_value': '3rd'
    };
    set_multiple_var(agent, jack_dict);
    
    let photo_url = `https://vignette.wikia.nocookie.net/jamescameronstitanic/images/e/ef/Untitledhgkjljlklk.png`;
    agent.add(new Card({
      	title: 'Jack Dawson',
        imageUrl: photo_url,
        text: `Jack has died from hypothermia`
      }));
    
    agent.add(new Suggestion(`survival prediction`));
    agent.add(new Suggestion(`passenger information`));
    }
  
  function rose_dewitt(agent) {
	let rose_dict = {
      'age': '17',
      'gender_value': 'female',
      'embarked': 'Southampton',
      'sibsp': '1',
      'parch': '1',
      'class_value': '1st'
    };
    set_multiple_var(agent, rose_dict);
   	let photo_url = `https://vignette.wikia.nocookie.net/jamescameronstitanic/images/d/d3/Rosedewittbukater.jpg/revision/latest?cb=20120518041253`;
    agent.add(new Card({
      title: 'Rose DeWitt Bukater',
      imageUrl: photo_url,
      text: 'Rose survived the catastrophe'
    }));
    agent.add(new Suggestion(`survival prediction`));
    agent.add(new Suggestion(`passenger information`));
  }

  // Run the proper function handler based on the matched Dialogflow intent name
  let intentMap = new Map();
  intentMap.set('Default Welcome Intent', welcome);
  intentMap.set('Default Fallback Intent', fallback);
  
  // general flow
  intentMap.set('problem_setting', problem_setting);
  intentMap.set('explain_feature', explain_feature);
  intentMap.set('list_variables', list_variables);
  intentMap.set('end_conversation', end_conversation);
  intentMap.set('restart', restart);
  intentMap.set('help_needed', help_needed);
  intentMap.set('current_knowledge', current_knowledge);
  intentMap.set('current_prediction', current_prediction);
  
  // telling info
  intentMap.set('specify_parch', specify_parch);
  intentMap.set('specify_sibsp', specify_sibsp); 
  intentMap.set('specify_age', specify_age);
  intentMap.set('specify_fare', specify_fare);

  intentMap.set('clear_variable', clear_variable);
  intentMap.set('multi_slot_filling', multi_slot_filling);
  
  intentMap.set('telling_age', age);
  intentMap.set('telling_gender', gender);
  intentMap.set('setting_embarked', setting_embarked);
  intentMap.set('setting_class', setting_class);
  intentMap.set('setting_fare', setting_fare);
  intentMap.set('setting_sibsp', setting_sibsp);
  intentMap.set('setting_parch', setting_parch);
  intentMap.set('travelling_alone', travelling_alone);
  
  // known passengers - shortcut
  intentMap.set('jack_dawson', jack_dawson);
  intentMap.set('rose_dewitt', rose_dewitt);
  intentMap.set('reset_rose', reset_rose);
  intentMap.set('reset_jack', reset_jack);
  
  // xAI
  intentMap.set('ceteris_paribus', ceteris_paribus);
  intentMap.set('how_to_survive', how_to_survive);
  intentMap.set('break_down', break_down);
  
  agent.handleRequest(intentMap);
});
