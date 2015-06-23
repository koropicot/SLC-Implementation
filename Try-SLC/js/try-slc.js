$(document).ready(function() {
	$('#submit').click(onSubmit);

	var calcCenter = function(){
		var h = $('#header').outerHeight();
		var f = $('#footer').outerHeight();
		var a = $('.container').height();

		$('#center').css('height','calc(100% - ' + (h+f) + 'px)');
	};
	calcCenter();

	$('#output').mCustomScrollbar({
		theme:'dark',
		alwaysShowScrollbar: 1,
		scrollbarPosition: 'outside'
	});

	$('textarea').each(function(){
		autosize(this);
	}).on('autosize:resized', calcCenter);

	//enable to input Tab and undo 
	$('#input').on('keydown', function(e) {
		if((e.keyCode || e.which) == 9){
			e.preventDefault();
			try{
				document.execCommand('insertText', false, '\t');
			}catch(ex){
				var v = this.value;
				var s = this.selectionStart;
				this.value = v.substring(0,this.selectionStart) + '\t' + v.substring(this.selectionEnd);
				this.selectionEnd = s + 1;
			}
		}
		if(e.shiftKey || e.ctrlKey){
			if((e.keyCode || e.which) == 13){
				e.preventDefault();
				onSubmit();
			}
		}
	})
});

function createOutputCard(code, output, error, name){
	var div = $('<div>', {'class': 'card'});

	if(error)
		div.addClass('error');

	if(name){
		$('#'+name).removeAttr('id');
		div.attr('id', name);
	}

	var pre = $('<p>', {'class': 'code', text: code});
	var p = $('<p>', {text: output});

	return div.append(pre).append(p);
}

var submit = getSubmit();

function onSubmit(){
	var input = $('#input').val();
	var ret = submit(input);
	$('#output .mCSB_container').append(ret);
	$('#input').val('');
	autosize.update($('#input'));
	$('#output').mCustomScrollbar('scrollTo', 'bottom', {scrollInertia:300});
}